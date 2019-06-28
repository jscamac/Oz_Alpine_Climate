#' Direct import temperature logger data
#' 
#' Direct import temperature logger data
#' @param temperature_data Dataframe. Current logger data
#' @param new_data Character. Path to directory containing new files.
#' @param chamber_metadata Character. Path to treatment meta data rds file containing dates when chambers were put on and off plots
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
import_newtempdata <- function(temperature_data,new_data, meta_data_path) {
  message("This function assumes sensor columns are in the following order: Ambient, Surface, Temp_5cm, Temp_10cm.
          If loggers have been manipulated please ensure sensors are still in correct order")
  dat <- temperature_data %>%
    dplyr::mutate(Site = as.character(Site),
                  Treatment = as.character(Treatment),
                  Hour = as.integer(Hour))
  metadat <- read.csv(meta_data_path)
  
  files <- list.files(new_data, pattern=".csv", full.names=TRUE)
  
  new <- lapply(files, function(x) {
    readr::read_csv(file = x,
                    skip = 1,
             col_names = c("Date","Time","Ambient","Surface","Temp_5cm", "Temp_10cm"),
             col_types = cols()) %>%
      dplyr::mutate( 
        Site = sub('_.*', '', basename(x)),
        Plot = as.numeric(sub('.*_(\\d+)_.*', '\\1', basename(x))),
        Treatment = sub('.*(.{3})\\.csv', '\\1', basename(x)),
        Datetime = lubridate::round_date(lubridate::fast_strptime(paste(Date,Time),
                                 format ='%d/%m/%y %H:%M:%S',lt = FALSE), Unit ="hour"),
        Date = as.Date(Datetime),
        Hour = lubridate::hour(Datetime)) %>%
      dplyr::select(Site, Plot, Treatment, Datetime, Date, Hour, Ambient, Surface, Temp_5cm, Temp_10cm)
    }) %>% 
      dplyr::bind_rows()
  
  
  # DATA CHECKS
  # Check site is correct
  if(!all(unique(new$Site) %in% metadat$Site)) {
    stop("Please check that site is spelt correctly for all observations. Only ITEX1U, ITEX2U, ITEX3B or ITEX4B accepted")
  }
  
  # Check Treatment is correct
  if(!all(do.call(paste, unique(new[, c('Site', 'Plot', 'Treatment')])) %in% 
          do.call(paste, unique(metadat[, c('Site', 'Plot', 'Treatment')])))) {
    stop("Please check that treatment is correctly assigned to all plots")
  }
  
  # Check logger/plot combination is correct.
  metalogger <- metadat[which(metadat$Logger==1),]
  if(!all(unique(paste0(new$Site,new$Plot)) %in% paste0(metalogger$Site,metalogger$Plot))) {
    stop("Some observations are from plots with no logger. Please double check data. If new logger installed, please add to the metadata file")
  }
  dplyr::bind_rows(dat, new)
}
