## Load Packages ## 
library(dplyr)

Forecast_boroughs = readRDS("Pollutant_forecasts_Sept2020.rds")
Pollutants_summarise = readRDS("Covidpollutants_pctshare.rds")

## Coding the EU Regulatory levels ##
## https://ec.europa.eu/environment/air/quality/standards.htm
Pollutants_summarise$Reg_level <- NA
Pollutants_summarise$Reg_level[which(Pollutants_summarise$parameter=="no2")] <- 40
Pollutants_summarise$Reg_level[which(Pollutants_summarise$parameter=="so2")] <- 125
Pollutants_summarise$Reg_level[which(Pollutants_summarise$parameter=="pm10")] <- 40
Pollutants_summarise$Reg_level[which(Pollutants_summarise$parameter=="pm25")] <- 25
Pollutants_summarise$Reg_level[which(Pollutants_summarise$parameter=="o3")] <- 120

Pollutants_summarise$Pct_change <- ((Pollutants_summarise$Point_Low-Pollutants_summarise$Point_High)/Pollutants_summarise$Point_High)*100
