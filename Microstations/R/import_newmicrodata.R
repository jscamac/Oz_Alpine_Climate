#' Direct import microstation data
#' 
#' Direct import microstation data
#' @param microstation_data Dataframe. Current microstation data
#' @param new_data Character. Path to directory containing new files.
#' @param chamber_metadata Character. Path to treatment meta data rds file containing dates when chambers were put on and off plots
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
import_newmicrodata <- function(microstation_data,new_data, meta_data_path) {
  
  dat <- microstation_data
  
  metadat <- read.csv(meta_data_path)
  
  files <- list.files(new_data, pattern=".csv", full.names=TRUE)

  new <- lapply(files, function(x) {
    read_csv(x, col_types = cols()) %>%
      rename_all(~sub('Time.*', 'Time', .x)) %>%
      rename_all(~sub('Temp.* ([^ ]+)\\)', 'Temp_\\1', .x)) %>%
      # ITEX1U_3 has an ambient temp sensor without being properly named ambient.
      rename_all(~sub('Temp_10284205', 'Temp_Ambient', .x)) %>% 
      rename_all(~sub('RH.*', 'RH', .x)) %>% 
      rename_all(~sub('DewPt.*', 'DewPt', .x)) %>% 
      rename_all(~sub('Water.* ([^ ]+)\\)', 'Water_\\1', .x)) %>% 
      # ITEX2.0_5 all sensors lost names (post May 2019)
      rename_all(~sub('Water_0', 'Water_10cmBG', .x)) %>%
      rename_all(~sub('Water_9833751', 'Water_5cmBG', .x)) %>%
      rename_all(~sub('Temp_9818210', 'Temp_Ambient', .x)) %>%
      rename_all(~sub('Temp_9824742', 'Temp_3cmBG', .x)) %>%
      dplyr::mutate(
        Datetime = lubridate::round_date(
          lubridate::fast_strptime(paste(Date,Time),
                                   format ='%d/%m/%y %H:%M:%S',lt = FALSE), 
          unit ="hour"),
        Date = as.Date(Datetime),
        Hour = lubridate::hour(Datetime),
        Site = sub('_.*', '', basename(x)),
        Plot = as.numeric(gsub('^.*_|\\D', '', basename(x))),
        Treatment = sub('.*(.{3})\\.csv', '\\1', basename(x)))
  }) %>%
    dplyr::bind_rows() %>% # This is a fixup to the depth mislabelling on the loggers
    dplyr::select(Site, Plot, Treatment, Datetime, Date, 
                  Hour, Temp_Ambient,RH, DewPt, 
                  Temp_3cmBG,
                  Moisture_10cmBG = Water_10cmBG, 
                  Moisture_3cmBG = Water_5cmBG)
  
  
  # DATA CHECKS
  # Check site is correct
  if(!all(unique(new$Site) %in% metadat$Site)) {
    stop("Please check that site is spelt correctly for all observations. Only ITEX1U or ITEX2.0 accepted")
  }
  
  # Fix issue where ITEX1U Plot 25 has incorrectly been assigned a CTL instead of OTC
  new <- new %>%
    dplyr::mutate(Treatment = ifelse(Site == "ITEX1U" & Plot == 25, "OTC", Treatment))
  
  # Check Treatment is correct
  if(!all(do.call(paste, unique(new[, c('Site', 'Plot', 'Treatment')])) %in% 
          do.call(paste, unique(metadat[, c('Site', 'Plot', 'Treatment')])))) {
    stop("Please check that treatment is correctly assigned to all plots")
  }
  
  # Check logger/plot combination is correct.
  metalogger <- metadat[which(metadat$Microstation==1),]
  if(!all(unique(paste0(new$Site,new$Plot)) %in% paste0(metalogger$Site,metalogger$Plot))) {
    stop("Some observations are from plots with no logger. Please double check data. If new logger installed, please add to the metadata file")
  }
  # Remove any duplicates
  dplyr::bind_rows(dat, new) %>% dplyr::distinct()
}
