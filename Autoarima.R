## Notes ##
## 2020-03-24 is the day after the UK government announced strict Covid-19 lockdown measures 
## Therefore, we have set this as the pre/post covid-19 cut off date 

## Clears the workspace ##
rm(list=ls())

## Libraries ##
library("dplyr")
library("ggplot2")
library("readxl")
library("zoo")
library("forecast")
library("data.table")
#library("reticulate")

## Set End date to yesterday ## 
Enddate = Sys.Date()-1 

## Import ##
Pollutants = readRDS("Pollutants_DF_Final.rds") ## ropenaqAPI.r 

## Set up ##
# Create prediction dataframe
createdate = function(Start, End){
  d = seq(as.Date(Start), as.Date(End), by="days")
  d <- as.data.frame(d)
  colnames(d)[1] <- "Date"
  return(d)
}

Date_forecast <- createdate(Enddate+1, "2020-12-31") ## Until end of 2020
Date_train <- createdate("2020-01-01", Enddate) ## API Data was run between these dates
Dim = nrow(Date_forecast)


## Loop over all 148 locations and 5 pollutants - scenarios ##
Forecast_boroughs <- NULL

for (i in (unique(Pollutants$location))) {
  print(i)
  for(k in (unique(Pollutants$parameter))) {
    print(k)
    
    print("Starting...")
    Trial <- Pollutants %>% filter(location==i & parameter==k)
    Trial <- as.data.frame(Trial)
    
    print("Checking...")
    if((nrow(Trial)>90) & any(Trial$dateLocal=="2020-03-24")) { ## At least 3 months of data (90 days)
      
    print("Yes")
    
    Trial$dateLocal <- as.Date(Trial$dateLocal)
    Trial$Dummy <- 1
    Trial$Dummy[1:which(Trial$dateLocal=="2020-03-24")] <- 0
    
    ## Fit an Auto Arima model ## 
    print("Modelling...")
    fit2 <- auto.arima(Trial$value_median, max.d=0, max.D=0, method="ML", xreg=Trial$Dummy)
    #checkresiduals(fit2)
    #autoplot(forecast(fit2))
    
    ## Refit ARMA Model to AR(1) if Order is 0,0,0 (flat line forecast)
    Order = transpose(as.data.frame((arimaorder(fit2))))
    if (sum(Order)==0){
      fit2 <- Arima(Trial$value_median, max.d=0, max.D=0, method="ML", xreg=Trial$Dummy)
    }
    
    ## Forecasts ##
    print("Forecasting...")
    y_hat_pred <- as.data.frame(forecast(fit2, h=Dim, xreg=rep(1, times=Dim)))
    Forecasts_bind <- cbind(Date_forecast, y_hat_pred[1])
    Data_forecast <- full_join(Trial, Forecasts_bind, by=c("dateLocal"="Date"))
    
    y_hat_pred_2 <- as.data.frame(forecast(fit2, h=Dim, xreg=rep(0, times=Dim)))
    Forecasts_bind <- cbind(Date_forecast, y_hat_pred_2[1])
    Data_forecast <- full_join(Data_forecast, Forecasts_bind, by=c("dateLocal"="Date"))
    
    ## Rename 
    colnames(Data_forecast)[10] <- "Point_Low"
    colnames(Data_forecast)[11] <- "Point_High"
    
    ## Add daily rolling average column 
    print("Formatting...")
    Data_forecast$value_mean_roll <- rollapply(Data_forecast$value_median, width=4, FUN = mean, align = "right", na.pad = TRUE) 
    
    Data_forecast$Point_High[which(Data_forecast$dateLocal==Enddate)] <- Data_forecast$value_mean_roll[which(Data_forecast$dateLocal==Enddate)]
    Data_forecast$Point_Low[which(Data_forecast$dateLocal==Enddate)] <- Data_forecast$value_mean_roll[which(Data_forecast$dateLocal==Enddate)]
    
    
    ## Add One day ahead 'In Sample' Forecast Line - 90 day rolling window ## 
    Today = which(Data_forecast$dateLocal==Enddate-1) ## Use two days ago to maximise data set
    print("Generate One step ahead errors...")
    
    if (length(Today!=0)){
      
      Insample <- NULL
      for (c in 91:Today) {
        #print(i)
        fit2 <- auto.arima(Trial$value_median[(c-90):(c-1)], max.d=0, max.D=0, method="ML")
        y_hat_pred <- as.data.frame(forecast(fit2, h=1))
        Temp <- cbind(Trial$dateLocal[c], y_hat_pred)
        Insample <- bind_rows(Insample, Temp)
      }
      
      ## Get the MAE and create new column ##
      Insample <- Insample[c(1:2)]
      colnames(Insample)[1] <- "dateLocal"
      colnames(Insample)[2] <- "90dayrolling"
      Data_forecast <- left_join(Data_forecast, Insample, by="dateLocal")
      Data_forecast$MAE <- mean(abs(Data_forecast$`90dayrolling`- Data_forecast$value_median), na.rm=TRUE)
      
    } else {
      
      Data_forecast$`90dayrolling` <- NA
      Data_forecast$MAE <- NA
    }

    print("Concatenating...")
    Forecast_boroughs <- rbind(Forecast_boroughs, Data_forecast)
    
    print("Fill in NAs...")
    Forecast_boroughs$location <- ifelse(is.na(Forecast_boroughs$location), i, Forecast_boroughs$location)
    Forecast_boroughs$parameter <- ifelse(is.na(Forecast_boroughs$parameter), k, Forecast_boroughs$parameter)
    
    
    } else {
      print("Doesn't conform to dimensions")
    }
    
  }
  
}

## Pre and Post Covid Level DF (Change in Pollutant Metric: September vs. March)
## Units and Pct decline 
Pollutants_summarise <- Forecast_boroughs %>% 
                        filter(dateLocal=="2020-12-31") %>%
                        mutate(Pct_change = (Point_High/Point_Low)*100) %>%
                        select(location, parameter, Point_Low, Point_High, Pct_change, MAE)

## Save as two DFs for Dashboard ## 
saveRDS(Forecast_boroughs, file = "Pollutant_forecasts_Sept2020.rds")
saveRDS(Pollutants_summarise, file = "Covidpollutants_pctshare.rds")
