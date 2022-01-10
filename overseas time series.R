library(fpp2)
library(readxl)
library(lubridate)
library(scales)
library(janitor)
library(timetk)
library(forecast)
library(pracma)
library(caret)
library(sos)
library(dplyr)
library(tidyverse)
library(fabletools)
library(fable)
library(tsibble)
library(plotly)
library(feasts)
library(ggplot2)


data <- read.csv("OverseasTrips.csv") %>% 
  clean_names() %>% 
  mutate(date = yq(i_quarter)) %>% 
  select(date,trips_thousands) %>% 
  tk_tbl()


Y <- ts(data[,2],start=c(2012,1),frequency=4)

autoplot(Y) + geom_point() +
  labs(title = "Overseas trips to Ireland by non-residents from Q1, 2012 to Q4, 2019") +
  ylab("Trips x 1000") +
  scale_x_continuous(n.breaks = 8)

# Data has a strong trend
# Transform data to get rid of trend

# Take the first difference of the data to remove the trend

DY <- diff(Y)

# Time Plot of differenced data

autoplot(DY) + geom_point() +
  labs(title = "Change in Overseas trips to Ireland by non-residents from Q1, 2012 to Q4, 2019") +
  ylab("Trips x 1000") +
  scale_x_continuous(n.breaks = 8)

# Series appears trend-stationary, use to investigate seasonality
# clear seasonal patterns in the data

ggseasonplot(DY) + 
    ggtitle("Seasonal Plot: Change in overseas trips") +
      ylab("Trips x 1000")


# another seasonal plot. This shows the changes by quarter
ggsubseriesplot(DY) + 
  ggtitle("Seasonal Plot: Change in overseas trips") +
  ylab("Trips x 1000")



############################################################
# Our series Y, has trend and seasonality
# To remove the trend, we take the first difference
# The first differenced series still has seasonality
#
# Forecast with various methods
############################################################
  
########
# Use a benchmark method to forecast
# Let's use the seasonal naive method as our benchmark.
# y_t = y_{t-s} + e_t e.g. value in Q1 2015 = value in Q1 2015 plus some random error
#######

fit <- snaive(DY)

# resid sd is how well the model is fitting, the closer to 0 the better
summary(fit) # Resid SD - 80.8587 missing on average by 80,500 visits

checkresiduals(fit) # gives us a sense of how well this model is fitting the data
# want the data to look random in the first plot
# ACF - want to see any autocorrelation and want all bars to be within the two blue lines (95% confidence interval)
# model looks good

sn_fcst <- forecast(fit,h=3)

autoplot(sn_fcst) +
  labs(title = "Change in Overseas trips to Ireland by non-residents from Q1, 2012 to Q4, 2019") +
    ylab("") +
      scale_x_continuous(n.breaks = 8)

print(summary(sn_fcst))

################
# Fit ETS method
################

fit_ets <- ets(Y)
summary(fit_ets) # Residual SD = 0.026 (Sigma)
checkresiduals(fit_ets) # 

ets_fcst <- forecast(fit_ets,h=3)
autoplot(ets_fcst)
print(summary(ets_fcst))

####################
# Fit on ARIMA model
# Model has to be stationary
####################

fit_arima <- auto.arima(Y,d=1,D=1,stepwise=FALSE, approximation = FALSE, trace = TRUE) 
# before fitting arima model you need to take the first difference to remove trend
# Can also get rid of seasonality by D=1
# Taking the regular difference and the seasonal difference in order to get rid of the trend and seasonality so our data is stationary

summary(fit_arima) # Returns the squared error of 6085 which is 78.00641
checkresiduals(fit_arima) # 

arima_fcst <- forecast(fit_arima,h=3)
autoplot(arima_fcst)
print(summary(arima_fcst))

##########################################
# Forecast with ETS
##########################################


fcst <- forecast(fit_ets,h=3)
autoplot(fcst)
print(summary(fcst))


# ME mean error - looking for smallest
# MAE - mean absolute error

# MODEL BUILDING
model_fit = data1 %>% 
  model(ARIMA = fit_arima,
        ETS = fit_ets,
        SAIVE = fit)

#TSLM(Year ~ trend(knots = lubridate::year("2006"))))

model_fit

## Diagnostics 
accuracy(model_fit)

data <- read.csv("OverseasTrips.csv") %>% 
  clean_names() %>% 
  mutate(date = yq(i_quarter)) %>% 
  select(date,trips_thousands) %>% 
  tk_tbl()


Y <- ts(data[,2],start=c(2012,1),frequency=4)

DY <- diff(Y)

fit <- snaive(DY)
fit_ets <- ets(Y)
fit_arima <- auto.arima(Y,d=1,D=1,stepwise=FALSE, approximation = FALSE, trace = TRUE) 

df = data %>% as_tsibble(index = date)
glimpse(df)


model_fit = df %>% 
  model(ARIMA = ARIMA(trips_thousands),
        ETS = ETS(trips_thousands),
        SNAIVE = SNAIVE(trips_thousands))




