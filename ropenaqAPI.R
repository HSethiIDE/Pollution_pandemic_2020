## Load libraries ## 
library(ropenaq)
library(dplyr)
library(revgeo)

## Set End date to yesterday ## 
End_date = Sys.Date()-1

## All Pollutants ('pm25', 'pm10', 'so2', 'no2', 'o3', 'co' and 'bc') 

Pollutants = c('pm25', 'pm10', 'so2', 'o3', 'no2')
Pollutants_DF <- NULL

## Loop over and Ingest Data ## 
for (i in Pollutants) {
  print(i)
  Temp <-data.frame(distinct(aq_measurements(country="GB", parameter = i, date_from = "2020-01-01", date_to = End_date)), stringsAsFactors = FALSE) %>% select(-country,-cityURL,-locationURL)
  Pollutants_DF <- bind_rows(Pollutants_DF, Temp)
  Sys.sleep(30)
}

Pollutants_DF_Final <- Pollutants_DF

## Collapse Dataframe by average daily pollutant level - Y mn obs 
Pollutants_DF_Final <- Pollutants_DF_Final %>% 
                       group_by(location, parameter, dateLocal, latitude, longitude) %>%
                       summarise(Value_mean = mean(value, na.rm=T), value_median = median(value, na.rm=T))

## Look up table to enable efficient geocoding ## 
y<-data.frame(distinct(aq_latest(country="GB", parameter = "no2")), stringsAsFactors = FALSE) %>% select(-country,-cityURL,-locationURL)

# Use revgeo to grab the nearest postcode, can use a different API e.g Google API 
Postcodes <- revgeo(longitude = y$longitude, latitude = y$latitude, output = "frame", item = "zip") %>% select(-housenumber,-street, -country, -city, -state)

# Bind postcodes 
Postcodes_mapping <- cbind(y, Postcodes) 
Postcodes_mapping <- Postcodes_mapping[c(11:13)]

Pollutants_DF_Final <- left_join(Pollutants_DF_Final, Postcodes_mapping, by=c("latitude", "longitude"))

## Save final geocoded Dataframe 
saveRDS(Pollutants_DF_Final, file = "Pollutants_DF_Final.rds")




