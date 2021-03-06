#import library
library(readr)
library(tidyr)
library(data.table)
library(tidyverse)
library(dplyr)
library(zoo)
library(tibble)

##preparing the time series sequence dataset for 5 clusters
#=====================================================================================
scans <- read_delim("./Data/scans_nc.txt", delim = ";", col_names = T)
cards <- read_delim("./Data/cards_nc.txt", delim = ";", col_names = T)

sca<-scans[,2:3]
sca<-unique(sca)
sca1<-merge(scans1, sca[, c("basket", "date")], by="basket")

sca2<-sca1[,c(2:3,12)]

#creat dataframe with time series
time_series <- sca2 %>%
  select(date, c6, card) %>% 
  spread(date, c6, fill = 0)

#remove the card name
time_series1 <- time_series[,2:844]
#filling all the NA's with 0

#transpose the dataframe
time_series_transpose<-as.data.frame(t(time_series1))

#filling the missing so as to keep the time-step of 1 day
time_step<-setDT(time_series_transpose, keep.rownames = TRUE)[]
time_step<-time_step %>%
  mutate(Date = as.Date(rn)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))

time_step<-time_step[-c(2)]
time_step1<-time_step

##transposing time_step again to make the time series dataframe==================
# first remember the names
n <- time_step$Date

# transpose all but the first column (name)
time_step <- as.data.frame(t(time_step[,-1]))
colnames(time_step) <- n

time_step<-cbind(card=time_series$card,time_step)
rownames(time_step)<-1:nrow(time_step)
#================================================================================

#filling all the NA's with previous cluster number in time_step==================
shopping_data<-time_step
max<-data.frame(max=apply(shopping_data,1,function(x) names(which.max(table(x)))))
shopping_data<-add_column(shopping_data,max,.after = "card")

#replacing NA's value
shopping_data<-na.locf(t(apply(shopping_data, 1, na.locf)))
shopping_data<-shopping_data[,-2]
shopping_data<-as.data.frame(shopping_data)

#not filling the NA values after the date last transaction was made


#================================================================================

rm(sca,sca1,sca2,time_series,time_series1,time_series_transpose,scans,cards,max,time_step1,time_step)
