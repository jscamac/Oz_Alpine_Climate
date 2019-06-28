#' Cleans microsation data
#' 
#' Cleans microsation data
#' @param rds_file_path Character. Path to rds file containing microclimate data from microstations
#' @param meta_dat_path Character. Path to rds file containing microclimate data metadata for microstations
#' @param subset_site Character. Name of site to subset too. Default = null (no subset).
#' @param include_raw_data Logical. To include raw/uncorrected data in final output. Default = FALSE.
#' @param wide_format Logical. To format data in wide format (i.e. one column per sensor). Default = FALSE.
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
#' 
clean_microclimate <- function(rds_file_path, meta_data_path, subset_site=NULL, include_raw_data=FALSE, wide_format = FALSE) {
  dat <- readRDS(rds_file_path)
  metadat <- read.csv(meta_data_path, stringsAsFactors = FALSE)
  
  if(!all(unique(dat$Site) %in% metadat$Site)) {
    stop("Please check that site is spelt correctly for all observations. Only ITEX1U or ITEX2.0 accepted")
  }
  
  if(!all(do.call(paste, unique(dat[, c('Site', 'Plot', 'Treatment')])) %in% 
          do.call(paste, unique(metadat[, c('Site', 'Plot', 'Treatment')])))) {
    stop("Please check that treatment is correctly assigned to all plots")
  }
  
  if(!is.null(subset_site)){
    dat <- dplyr::filter(dat, Site==subset_site)
    metadat <- dplyr::filter(metadat, Site==subset_site)
  }
  metalogger <- metadat[which(metadat$Microstation==1),]
  if(!all(unique(paste0(dat$Site,dat$Plot)) %in% paste0(metalogger$Site,metalogger$Plot))) {
    stop("Some observations are from plots with no logger. Please double check data. If new logger installed, please add to the metadata file")
  }
  
  # Removes data that is errenous due to loggers failing or battery power being too low.
  # Plot 6 has been malfunctioning since October 2011. As such the data collected from this logger is deemed unusable.
  # Plot 2 and 19 have had on going battery issues. Most of the data recorded appears correct but occassionally
  # the day before the logger batteries die the sensors error.
  # Processing rules are based on minimum and maximum temperatures observed from other nearby sites & expert judgement.
  # We purposely didn't make the rules too strict so that rare extremes were not removed.
  dat <- dat %>%
    dplyr::mutate(CTemp_Ambient = ifelse(Temp_Ambient > 50 |
                                           Temp_Ambient < - 20 |
                                           Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                           Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                           Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                           Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                           Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                           Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                           Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19, 
                                         NA, Temp_Ambient),
                  CTemp_3cmBG = ifelse(Temp_3cmBG > 80 | 
                                         Temp_3cmBG < -20 |
                                         Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                         Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                         Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                         Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                         Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                         Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                         Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19, 
                                       NA, Temp_3cmBG),
                  CRH = ifelse(Temp_Ambient > 50 |
                                 Temp_Ambient < -20 |
                                 Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                 Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                 Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                 Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                 Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                 Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                 Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19,
                               NA, RH),
                  CDewPt = ifelse(DewPt < -50 |
                                    DewPt > 50 |
                                    Temp_Ambient > 50 |
                                    Temp_Ambient < -20 |
                                    Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                    Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                    Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                    Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                    Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                    Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                    Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19, 
                                  NA, DewPt),
                  CMoisture_3cmBG = ifelse(Moisture_3cmBG < 0 |
                                             Moisture_3cmBG > 1 |
                                             Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                             Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                             Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                             Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                             Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                             Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                             Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19,  
                                           NA, Moisture_3cmBG*100),
                  CMoisture_10cmBG = ifelse(Moisture_10cmBG < 0 |
                                              Moisture_10cmBG > 1 |
                                              Date >= '2011-10-01' & Site == 'ITEX2.0' & Plot == 6 |
                                              Date == '2011-10-17' & Site == 'ITEX2.0' & Plot == 2 |
                                              Date == '2015-04-30' & Site == 'ITEX2.0' & Plot == 2 |
                                              Date == '2014-11-11' & Site == 'ITEX2.0' & Plot == 19 |
                                              Date == '2015-03-13' & Site == 'ITEX2.0' & Plot == 19 |
                                              Date >= '2012-03-10' & Date <= '2012-04-01' & Site == 'ITEX2.0' & Plot == 2 |
                                              Date >= '2013-10-19' & Date <= '2013-10-24' & Site == 'ITEX2.0' & Plot == 19,
                                            NA, Moisture_10cmBG*100),
                  CMoisture3to10cmBG = (CMoisture_3cmBG + CMoisture_10cmBG)/2) %>%
    dplyr::select(-c(Date, Hour)) %>% # No longer needed as gets recalulated later
    tidyr::gather(Sensor, Clim_value, -Site, -Plot, -Treatment,-Datetime) %>%
    dplyr::group_by(Site, Plot, Treatment, Datetime, Sensor) %>%
    dplyr::summarise(Clim_value = mean(Clim_value, na.rm=TRUE)) %>%
    dplyr::ungroup()
  
  all_dates <- data.frame(Datetime = seq.POSIXt(range(dat$Datetime)[1], range(dat$Datetime)[2], 'hour'))
  
  meta <- select(metalogger, Site, Plot, Treatment)
  meta <- meta[rep(seq_len(nrow(meta)), each=nrow(all_dates)),] %>%
    dplyr::mutate(Datetime = rep(all_dates$Datetime, nrow(metalogger)))
  
  meta <- meta[rep(seq_len(nrow(meta)), each=length(unique(dat$Sensor))),] %>%
    dplyr::mutate(Sensor = rep(unique(dat$Sensor), length.out=n()))
  
  cleaned <- left_join(meta, dat, by = c("Site", "Plot", "Treatment", "Datetime", "Sensor")) %>%
    dplyr::mutate(
      Date = as.Date(Datetime),
      Hour = hour(Datetime),
      Clim_value = ifelse(is.nan(Clim_value),NA,Clim_value)) %>%
    dplyr::select(Site, Plot,Treatment,Datetime,Date,Hour,Sensor,Clim_value) %>%
    dplyr::arrange(Site,Sensor,Plot, Datetime) %>%
    setNames(tolower(names(.)))
  
  if(include_raw_data==FALSE) {
    cleaned <- dplyr::filter(cleaned, !sensor %in% c('Temp_Ambient','RH','DewPt', 'Temp_3cmBG','Moisture_3cmBG', 'Moisture_10cmBG'))
  }
  
  if(wide_format == TRUE) {
    cleaned <- tidyr::spread(cleaned, sensor, clim_value)
  }
  
  cleaned
}