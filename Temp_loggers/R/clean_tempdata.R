#' Cleans 4-channel temperature logger data
#' 
#' Cleans 4-channel temperature logger data
#' @param rds_file_path Character. Path to rds file containing temperature data
#' @param meta_dat_path Character. Path to csv file containing temperature plot meta data
#' @param subset_site Character. Name of site to subset too. Default = null (no subset). 
#' @param include_raw_data Logical. To include raw/uncorrected data in final output (Default = FALSE).
#' @param wide_format Logical. To format data in wide format (i.e. one column per sensor). Default = FALSE
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
#' 
clean_tempdata <- function(rds_file_path, meta_data_path, subset_site=NULL, include_raw_data=FALSE, wide_format = FALSE) {
  dat <- readRDS(rds_file_path)
  metadat <- read.csv(meta_data_path, stringsAsFactors = FALSE)
  
  if(!all(unique(dat$Site) %in% metadat$Site)) {
    stop("Please check that site is spelt correctly for all observations. Only ITEX1U, ITEX2U, ITEX3B or ITEX4B are accepted")
  }
  
  if(!all(do.call(paste, unique(dat[, c('Site', 'Plot', 'Treatment')])) %in% 
          do.call(paste, unique(metadat[, c('Site', 'Plot', 'Treatment')])))) {
    stop("Please check that treatment is correctly assigned to all plots")
  }
  
  if(!is.null(subset_site)){
    dat <- dplyr::filter(dat, Site==subset_site)
    metadat <- dplyr::filter(metadat, Site==subset_site)
  }
  metalogger <- metadat[which(metadat$Logger==1),]
  if(!all(unique(paste0(dat$Site,dat$Plot)) %in% paste0(metalogger$Site,metalogger$Plot))) {
    stop("Some observations are from plots with no logger. Please double check data. If new logger installed, please add to the metadata file")
  }
  
  # Processing rules are based on minimum and maximum temperatures observed from other nearby sites & expert judgement.
  # We purposely didn't make the rules too strict so that rare extremes were not removed.
  dat <- dat %>%
    dplyr::mutate(CAmbient_Temp = ifelse(Ambient > 50 |
                                           Ambient < - 20, NA, Ambient),
                  CSurface = ifelse(Surface > 80 | 
                                      Surface < -20, NA, Surface),
                  CTemp_5cm = ifelse(Temp_5cm > 30 | 
                                       Temp_5cm < -5, NA, Temp_5cm),
                  CTemp_10cm = ifelse(Temp_10cm > 30 | 
                                        Temp_10cm < -2, NA, Temp_10cm)) %>%
    tidyr::gather(Sensor, Clim_value, -Site, -Plot, -Treatment, -Datetime,-Date, -Hour) %>%
    dplyr::group_by(Site, Plot, Treatment, Datetime, Sensor) %>%
    dplyr::summarise(Clim_value = mean(Clim_value, na.rm=TRUE)) %>%
    dplyr::ungroup()
  
  all_dates <- data.frame(Datetime = seq.POSIXt(range(dat$Datetime)[1], range(dat$Datetime)[2], 'hour'))
  
  meta <- dplyr::select(metalogger, Site, Plot, Treatment)
  meta <- meta[rep(seq_len(nrow(meta)), each=nrow(all_dates)),] %>%
    dplyr::mutate(Datetime = rep(all_dates$Datetime, nrow(metalogger)))
  
  meta <- meta[rep(seq_len(nrow(meta)), each=length(unique(dat$Sensor))),] %>%
    dplyr::mutate(Sensor = rep(unique(dat$Sensor), length.out=n()))
  
  cleaned <- dplyr::left_join(meta, dat, by = c("Site", "Plot", "Treatment", "Datetime", "Sensor")) %>%
    dplyr::mutate(
      Date = as.Date(Datetime),
      Hour = lubridate::hour(Datetime),
      Clim_value = ifelse(is.nan(Clim_value),NA,Clim_value)) %>%
    dplyr::select(Site, Plot,Treatment,Datetime,Date,Hour,Sensor,Clim_value) %>%
    dplyr::arrange(Site,Sensor,Plot, Datetime) %>%
    setNames(tolower(names(.)))
  
  if(include_raw_data==FALSE){
    cleaned <- dplyr::filter(cleaned, !sensor %in% c('Ambient','Surface','Temp_5cm', 'Temp_10cm'))
  }
  
  if(wide_format == TRUE) {
    cleaned <- tidyr::spread(cleaned, sensor, clim_value)
  }
  cleaned
}