#' Cleans weatherstation data
#' 
#' Cleans weatherstation data
#' @param rds_file_path Character. Path to rds file containing weatherstation data
#' @param include_raw_data Logical. To include raw/uncorrected data in final output. Default = FALSE.
#' @param wide_format Logical. To format data in wide format (i.e. one column per sensor). Default = FALSE.
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
#' 
clean_weatherdata <- function(rds_file_path, include_raw_data=FALSE, wide_format = FALSE) {
  dat <- readRDS(rds_file_path)
  
  dat <- dat %>%
    dplyr::mutate(CTemp_Ambient = ifelse(Temp_Ambient > 40 | 
                                           Temp_Ambient < - 20, NA, Temp_Ambient),
                  CTemp_Surface = ifelse(Temp_Surface > 80 | 
                                           Temp_Surface < -20, NA, Temp_Surface),
                  CTemp_5cmBG = ifelse(Temp_5cmBG > 30 | 
                                         Temp_5cmBG < -5, NA, Temp_5cmBG),
                  CTemp_10cmBG = ifelse(Temp_10cmBG > 30 | 
                                        Temp_10cmBG < -2, NA, Temp_10cmBG),
                  CTemp_15cmBG = ifelse(Temp_15cmBG > 30 | 
                                        Temp_15cmBG < -2, NA, Temp_15cmBG),
                  CRH = ifelse(RH >100 | RH < 0 | Temp_Ambient > 40 | Temp_Ambient < -20, NA, RH),
                  CDewPt = ifelse(DewPt < -50 | DewPt > 50 |
                                    Temp_Ambient > 40 | Temp_Ambient < -20, NA, DewPt),
                  CMoisture_5cmBG = ifelse(Moisture_5cmBG < 0 | Moisture_5cmBG > 1, 
                                           NA, Moisture_5cmBG*100),
                  CRain_mm = ifelse(Rain_mm > 200 | Rain_mm < 0, NA, Rain_mm),
                  CPAR = ifelse(PAR > 2500 | PAR < 0, NA, PAR),
                  CWindSpeed_ms = ifelse(WindSpeed_ms > 76 | WindSpeed_ms < 0, NA, WindSpeed_ms),
                  CGustSpeed_ms = ifelse(GustSpeed_ms > 76 | GustSpeed_ms < 0, NA, GustSpeed_ms),
                  CWind_Direction = ifelse(Wind_Direction > 360 | Wind_Direction < 0, NA, Wind_Direction)) %>%
    tidyr::gather(Sensor, Clim_value, -Datetime,-Date, -Hour) %>%
    dplyr::group_by(Datetime, Sensor) %>%
    dplyr::summarise(Clim_value = mean(Clim_value, na.rm=TRUE)) %>%
    dplyr::ungroup()
  
  all_dates <- data.frame(Datetime = seq.POSIXt(range(dat$Datetime)[1], range(dat$Datetime)[2], 'hour'))
  
  
  
  meta <- data.frame(Sensor = rep(unique(dat$Sensor), each = nrow(all_dates)),
                     Datetime = rep(all_dates$Datetime, times =length(unique(dat$Sensor))),
                     stringsAsFactors = FALSE)
  
  cleaned <- left_join(meta, dat, by = c( "Sensor","Datetime")) %>%
    dplyr::mutate(
      Date = as.Date(Datetime),
      Hour = hour(Datetime),
      Clim_value = ifelse(is.nan(Clim_value),NA,Clim_value)) %>%
    dplyr::select(Datetime,Date,Hour,Sensor,Clim_value) %>%
    dplyr::arrange(Datetime) %>%
    setNames(tolower(names(.)))
  
  if(include_raw_data==FALSE) {
    cleaned <- dplyr::filter(cleaned, !sensor %in% c('Temp_Ambient','RH','DewPt',
                                                     'Temp_Surface', 'Temp_5cmBG',
                                                     'Temp_10cmBG', 'Temp_15cmBG',
                                                     'PAR', 'Wind_Direction','Rain_mm',
                                                     'Moisture_5cmBG',
                                                     'WindSpeed_ms', "GustSpeed_ms"))
  }
  
  if(wide_format == TRUE) {
    cleaned <- tidyr::spread(cleaned, sensor, clim_value)
  }
  
  cleaned
}