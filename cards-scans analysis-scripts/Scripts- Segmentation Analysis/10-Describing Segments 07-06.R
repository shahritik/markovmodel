#Describing the segments

setwd("C:/Users/pc/Desktop/Ritik/MBS-Internship/Cards & Scans Analysis")

library(readr)
library(flexclust)
library(dplyr)
library(plyr)
scans <- read_delim("./Data/scans_nc.txt", delim = ";", col_names = T)
cards <- read_delim("./Data/cards_nc.txt", delim = ";", col_names = T)
incidence <- read_delim("./Data/incidence_nc.txt", delim = ";", col_names = T)
incidence_mat <- as.matrix(incidence[, -1])
# manually input k and best seed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

k <- 5
best_seed <- 227
file_name <- paste0("k=", k, "_seed=", best_seed)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set up global parameters for clustering ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fc_cont <- new("flexclustControl") ## flexclustControl object holds "hyperparameters"
fc_cont@tolerance <- 0.1 ## kcca only uses if classify == "weighted"
fc_cont@iter.max <- 30
fc_cont@verbose <- 0
fc_family <- "ejaccard" ## distance metric, Jaccard distance w/ centroid means

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# invoke kcca with the best seed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(best_seed)

# perform cluster analysis
cli <- kcca(incidence_mat, k = k, save.data = TRUE,
            control = fc_cont, family = kccaFamily(fc_family))

#storing the segment membership in c6

c6<- clusters(cli)

#getting the size of each market segment
#table(c6)

#makign the dataframe consiting of unique baskets, cardnumbers
#total euros, total units and number of categories
scans1<-scans[,c(1,3)]
scans1<-distinct(scans1[order(scans1$basket),])
scans2<-scans
scans2$barcode<-NULL
scans2$date<-NULL
scans2$category<-NULL
scans2$card<-NULL
scans3<-data.frame(xtabs(formula=euros~basket, data=scans2))
scans4<-data.frame(xtabs(formula=units~basket, data=scans2))
scans5<-scans
scans5$card<-NULL
scans5$date<-NULL
scans5$barcode<-NULL
scans5$units<-NULL
scans5$euros<-NULL
scans6<-ddply(scans5,~basket,summarise,num_of_cat=length(unique(category)))
scans_day<-scans
scans_day$day= weekdays(as.POSIXct(scans$date), abbreviate = F)
scans_day <- scans_day[ -c(1:2,4:7) ]
scans_day<-unique(scans_day)

#Adding segement number to each basket
scans1$c6<-as.factor(c6)

#Adding gender to each baket by comparing the card numbers 
scans1<-merge(scans1, cards[, c("card", "gender")], by="card")

#Adding age to each basket by comparing the card numbers
scans1<-merge(scans1, cards[, c("card", "age")], by="card")

#Adding total euros to each basket
scans1<-merge(scans1, scans3[, c("basket", "Freq")], by="basket")
colnames(scans1)[6]<-"euros"

#Adding total number of units to each basket
scans1<-merge(scans1, scans4[, c("basket", "Freq")], by="basket")
colnames(scans1)[7]<-"units"

#Adding number of unique categories to each basket 
scans1<-merge(scans1, scans6[, c("basket", "num_of_cat")], by="basket")

#Adding hh_size to each basket
scans1<-merge(scans1, cards[, c("card", "hh_size")], by="card")

#Adding the day of the week
scans1<-merge(scans1, scans_day[, c("basket", "day")], by="basket")
mapping <- c("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7)
scans1$day1 <- mapping[scans1$day]

#Getting the number of males and females for each segment
C6.gender <- with(scans1,table("Segment number" = c6, gender))
C6.gender

#Getting the age of people in each segment
C6.age <- with(scans1,table("Segment number" = c6, age))
C6.age

#barchart for number of male and female in each segment 
barchart(C6.gender)

rm(C6.age)
rm(C6.gender)
rm(scans2,scans3,scans4,scans5,scans6,scans_day)
rm(scans,cards)
rm(cli,fc_cont,incidence,incidence_mat)
rm(best_seed,fc_family,file_name,k)
