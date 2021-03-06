#import library
library(readr)
library(data.table)
library(PSF)

shopping_data<-time_step1
shopping_data<-as.data.frame(shopping_data)

#converting all NA's to 0
for (i in 2:337) {
  shopping_data[,i] = factor(shopping_data[,i], levels=c(levels(shopping_data[,i]), 0))
  shopping_data[,i][is.na(shopping_data[,i])] = 0
}

#converting factor type to interger type for all columns
shopping_data1<-shopping_data[,2:337]
shopping_data1[] <- lapply(shopping_data1, function(x) as.numeric(as.character(x)))

#scaling the data
shopping_data_train<-scale(shopping_data1)

#===========================================================================
#diving the dataset into train and test
train<-shopping_data_train[1:970,307]
test<-shopping_data_train[971:1002,307]

#applying 3 prediction models
#Pattern Sequence Forecasting (PSF), Arima, ETS
PSF <- NULL
ARIMA <- NULL
ETS <- NULL

for(i in 1:5)
{
  set.seed(i)
  
  # for PSF
  psf_model <- psf(train)
  a <- predict(psf_model, n.ahead = 32)
  
  # for ARIMA
  arima_model<-auto.arima(train)
  b <- forecast(arima_model, 32)$mean
  
  # for ets
  c <- as.numeric(forecast(ets(train), 32)$mean)
  
  ## For Error Calculations
  # Error for PSF
  PSF[i] <- sqrt(mean((test - a)^2))
  # Error for ARIMA
  ARIMA[i] <- sqrt(mean((test - b)^2))
  # Error for ETS
  ETS[i] <- sqrt(mean((test - c)^2))
  
}

#comapring the predicted and the test value
new_data<-data.frame(test,a)

#printing the mean error
PSF
mean(PSF)
ARIMA
mean(ARIMA)
ETS
mean(ETS)

plot(psf_model,a)
#============================================================================


