# Climate data collected on the Bogong High Plains, Victoria, Australia

## Summary

This data repository contains various climatic data collected on the Bogong High Plains, Victoria, Australia from 2004 onwards.

Broadly, these data fall into four types

1. **4-Channel Onset/Hobo temperature loggers** (see folder `Temp_loggers`) set up in 2004 as part of the Australian International Tundra Experiment (OZTEX). These data contain:
	-	Ambient temperature (5cm above ground);
	-	Surface temperature;
	-	Soil temperature (5cm below ground); And
	-	Soil temperature (10cm below ground).

The majority of the measurements are taken hourly in Degrees Celsius. The loggers were initially positioned at two unburnt sites (ITEX1U and ITEX2U), and two burnt sites (ITEX3B and ITEX4B) in Open Top Chambers (Warmed) and Control plots. Unburnt sites contained 8 per site (4 in OTCs and 4 in Controls)). Burnt sites contained only 4 loggers per site (2 OTC and 2 Control). However, the total number in sites diminished over time as loggers began to mulfunction and sites were decomissioned. As of 2019, these loggers are now only maintained at ITEX1U. Gaps amongst records are due to logger malfunctions. More details about these sites can be found [here]{https://link.springer.com/article/10.1007/s00442-015-3261-2} and [here]{https://www.publish.csiro.au/bt/BT08018}. The compiled (unclean) version of these data can loaded directly using `temp_loggers <- readRDS("Temp_loggers/TempLogger.rds")`

2.	**Onset Microstation loggers** (see folder `Microstations`) has been collected since November 2010 as part of the ITEX experiment and Dr James Camac's PhD. These data contain:
	-	Ambient temperature (`Temp_Ambient`; 10 cm above ground in stevenson screen);
	-	Relative humidity (`RH`; 10 cm above ground in stevenson screen);
	-	Dew Point (`DewPt`; 10 cm above ground in stevenson screen);
	-	Soil temperature (`Temp_3cmBG`; 3 cm below ground);
	- 	Soil moisture 3 cm below ground (`Moisture_3cmBG`; in Volumetric Water Content); And
	- 	Soil moisture 10 cm below ground (`Moisture_10cmBG`: inVolumetric Water Content)

These measurements were mostly taken at hourly intervals from ITEX1U (an unburnt ITEX site) and a new OTC shrub seedling experiment conducted in experimentally burnt site (For specific information see [here]{https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.13614} dubbed ITEX2.0. ITEX2.0 contains the full assortment of climatic data, however, ITEX1U does not contain soil temperature data. Again, gaps in the data exist due to logger malfunctions. As of 2019 these loggers are still recording climatic data from both sites. The compiled (unclean) version of these data can be loaded directly in R using `microclimate <- readRDS("Microstations/Microstations.rds")`. **Note** Within the raw datafiles Moisture_3cmBG was incorrectly assigned as Moisture_5cmBG. This has been fixed within the compiled data.

3.	**Onset Weather station** was set up in 2004 originally at ITEX2U (See folder `WeatherStation`). However, it was moved to ITEX3B for a short period due to vandalism/feral horse traffic. The weather station recorded at hourly intervals:
	-	Ambient temperature (`Temp_Ambient`; 1.2 m above ground in stevenson screen);
	-	Relative humidity (`RH`; 1.2 m above ground in stevenson screen);
	-	Dew Point (`DewPt`; 1.2 m above ground in stevenson screen);
	-	Photosynthetic Active Radiation (`PAR`; at approx 1.2m above ground)
	-	Rainfall (`Rain_mm`);
	-	Soil surface temperature (`Temp_surface`);
	-	Soil temperature 5 cm below ground (`Temp_5cmBG`);
	-	Soil temperature 10 cm below ground (`Temp_10cmBG`);
	-	Soil temperature 15 cm below ground (`Temp_15cmBG`);
	- 	Soil moisture 5 cm below ground (`Moisture_5cmBG`;Volumetric Water Content); 
	- 	Wind direction (`Wind_Direction`);
	-	Wind speed (`WindSpeed_ms` in metres per second); And
	-	Gust speed (`GustSpeed_ms` in metres per second)

	The weather station data contains multiple large data gaps due to malfunctions and various other issues. As of June 2019, the last recording of weather station data occured in April 2018 after which the weather station was struck by lightening. We are now in the process of replacing the weatherstation. These data can be loaded into R using `weatherstation <- readRDS("WeatherStation/Weatherstation.rds")`

4.	**Burnt heathland temperature data** (See folder `Burnt_Heathland_ibuttons`). This dataset contains hourly temperature recordings at three burnt closed heathland plots on the Bogong High Plains from November 2011 through to April 2013.

5.	**OTC Dates** (See folder OTC_dates). This folder contains the dates in which Open Top Chambers (OTCs) were placed ontop of the plots at the start of spring and removed at the beginning of winter. These dates are relevant to both Microstation and Temperature logger data.


## Obtaining cleaned versions (erroneous records removed) of weather station, temperature loggers or microclimate data

The compiled .rds files contain the raw compile data that has not been screened for various errors. If you wish to obtain cleaned versions of these data please use follow the instructions below:

### Load dependent R packages

The cleaning functions developed below require the user to install and load `dplyr`(0.8.0.1) `readr` (1.3.1) `tidyr` (0.8.1) and `lubridate` (1.7.4). Note due to regular changes in these packages we cannot gaurentee they will work with later (or earlier) versions of the above packages.

If these packages do not exist on your computer, they can be installed in R using:

```
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")
```

They can then be load in R using:
```
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
```

Depending on the data you wish to clean the following code can be run:

### Clean weather station data

```
source("WeatherStation/R/clean_weatherstation.rds.R")
weather_clean <- clean_weatherdata(rds_file_path = "WeatherStation/Weatherstation.rds") 
```

By default the above code will return a long data frame with only the cleaned versions of the data. If you wish to retain the raw data or would prefer the data to be returned in wide format (i.e. a sensor per column), please examine the arguments of `clean_weatherdata`.


### Clean Temperature logger data

```
source("Temp_loggers/R/clean_tempdata.R")
temp_clean <- clean_tempdata(rds_file_path = "Temp_loggers/TempLogger.rds",
                             meta_data_path = "Temp_loggers/meta_data/TempLogger_meta.csv")
```

By default the above code will return a long data frame with only the cleaned versions of the data. If you wish to retain the raw data or would prefer the data to be returned in wide format (i.e. a sensor per column), please examine the arguments of `clean_tempdata`.

### Clean Microclimate data

```
source("Microstations/R/clean_microclimate.R")
micro_clean <- clean_microclimate(rds_file_path = "Microstations/Microstations.rds",
                                  meta_data_path = "Microstations/meta_data/microstation_metadata.csv")
```


## Incorporating new data

New Microstation and Temperature logger data can readily be added to existing compiled .rds files using the following code:


### New weather stations data

```

new_micro <- import_newmicrodata(microstation_data = readRDS("Microstations/Microstations.rds"),
								 new_data = "Microstations/raw_data/new_directory/",
								 meta_data_path = "Microstations/meta_data/microstation_metadata.csv")
```

Here `"Microstations/raw_data/new_directory/"` is the path to the directory containing .csv files from the HOBO loggers. Please ensure the filenames are consistent (e.g. ITEX2.0_5CTL.csv).


If you are happy with the compilation, the existing .rds file can be overwritten using:
```
saveRDS(new_micro, "Microstations/Microstations.rds")
```


### New temperature logger data

```
dat <- readRDS("microstations/microstation_data_May19.rds")

new_micro <- import_newmicrodata(temperature_data = readRDS("Temp_loggers/TempLogger.rds"),
								 new_data = "Temp_logger/raw_data/new_directory/",
								 meta_data_path = "Temp_logger/meta_data/TempLogger_meta.csv")
```

Here `"Temp_logger/raw_data/new_directory/"` is the path to the directory containing .csv files from the HOBO loggers. Please ensure the filenames are consistent (e.g. ITEX1U_10_OTC.csv).


If you are happy with the compilation, the existing .rds file can be overwritten using:
```
saveRDS(new_micro, "Temp_loggers/TempLogger.rds")
```


