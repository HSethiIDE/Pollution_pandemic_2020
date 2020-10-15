rm(list=ls())

library("dplyr")
library("ggplot2")

## http://wiki.analytica.com/Choosing_an_appropriate_distribution
## https://www.countbayesie.com/blog/2015/3/3/6-amazing-trick-with-monte-carlo-simulations

Forecast_boroughs = readRDS("Pollutant_forecasts_Sept2020.rds")

NO2_data <- Forecast_boroughs %>% filter(parameter=="no2" & !is.na(value_median)) %>% select(location, dateLocal, value_median)
NO2_data$Lockdown <- ifelse(NO2_data$dateLocal<"2020-03-24", "0", "1")

## Filter ##
NO2_data_precovid <- NO2_data %>% filter(Lockdown == "0") 
NO2_data_postcovid <- NO2_data %>% filter(Lockdown == "1") 

## Fit distributions via ggplot ##
## Positive skewed distributions ## 
ggplot() + 
  geom_density(data=NO2_data, aes(x=value_median, group=Lockdown, fill=Lockdown),alpha=0.5, adjust=2) + 
  xlab("ug/mg3") +
  ylab("Density") +
  theme_classic() +
  geom_vline(xintercept = 40, linetype="dotdash")

## Calculate number of times > 40 
Breaches <- NO2_data %>% group_by(Lockdown) %>% 
            mutate(Breach = if_else(value_median>40, 1, 0)) %>% 
            summarise(Total = n(), Breaches = sum(Breach)) %>% 
            mutate(Pct = Breaches/Total)

mean(NO2_data_precovid$value_median)
sd(NO2_data_precovid$value_median)

Mean = mean(NO2_data_postcovid$value_median) ## 12.7
SD = sd(NO2_data_postcovid$value_median) ## 10.65

## Run 100 Monte Carlo Simulations on Gamma Distribution ## 
## Useful for scenario analysis and extrapolating data from smaller samples to larger populations/time frames ## 
## https://homepage.divms.uiowa.edu/~mbognar/applets/gamma.html

# R program to plot gamma distribution (Post-Covid)
## https://stats.stackexchange.com/questions/342639/how-to-find-alpha-and-beta-from-a-gamma-distribution

Gamma_1000 <- NULL
EUReg <- NULL

for (i in 1:5000) {
# Set seed for reproducibility 
print(i)
set.seed(i) 

# Specify sample size (November + December days (61)*150 stations)
N <- 61*150

## Calculate Parameters ##
Shape = Mean^2/SD^2
Scale = 1/(Mean/SD^2)

# Draw N gamma distributed values 
y_rgamma <- rgamma(N, shape = Shape, scale=Scale)  

# Plot of randomly drawn gamma density 
Distribution <- as.data.frame(y_rgamma)

ggplot() + 
  geom_density(data=Distribution, aes(x=y_rgamma, fill="Simulation"),alpha=0.5, adjust=2) + 
  xlab("ug/mg3") +
  ylab("Density") +
  theme_classic() +
  geom_vline(xintercept = 40, linetype="dotdash")

## Calculate probability X>40  
Shape = mean(Distribution$y_rgamma)^2/sd(Distribution$y_rgamma)^2
Scale = 1/(mean(Distribution$y_rgamma)/sd(Distribution$y_rgamma)^2)

Temp = as.data.frame(1-pgamma(q = 40, shape = Shape, scale = Scale))

## Calculate probability X<1
## pgamma(q = 1, shape = Shape, scale = Scale)

Gamma_1000 <- bind_cols(Distribution, Gamma_1000)
EUReg <- bind_rows(Temp, EUReg)
}

colnames(EUReg)[1] <- "Prob_above_40"

## Print Max value across 5000 simulations ## 
max(Gamma_1000) ## 135!
max(NO2_data_postcovid$value_median) ## 255

ggplot() + 
  geom_density(data=EUReg, aes(x=Prob_above_40),alpha=0.5) + 
  xlab("Probability exceeds EU limit") +
  ylab("Density") +
  theme_classic() 

