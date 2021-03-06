#import library
library(TraMineR)
library(readr)
library(tidyr)
library(data.table)
library(varhandle)
library(cluster)

##preparing the shopping_data dataset for 5 clusters
#=====================================================================================
scans <- read_delim("./Data/scans_nc.txt", delim = ";", col_names = T)
cards <- read_delim("./Data/cards_nc.txt", delim = ";", col_names = T)

#naming the segments
for (i in 2:ncol(shopping_data)) {
  levels(shopping_data[,i]) <- c(levels(shopping_data[,i]), "general")
  shopping_data[,i][shopping_data[,i] == '1'] <- 'general'
  levels(shopping_data[,i]) <- c(levels(shopping_data[,i]), "milk")
  shopping_data[,i][shopping_data[,i] == '2'] <- 'milk'
  levels(shopping_data[,i]) <- c(levels(shopping_data[,i]), "paper and health")
  shopping_data[,i][shopping_data[,i] == '3'] <- 'paper and health'
  levels(shopping_data[,i]) <- c(levels(shopping_data[,i]), "vegetables and fruits")
  shopping_data[,i][shopping_data[,i] == '4'] <- 'vegetables and fruits'
  levels(shopping_data[,i]) <- c(levels(shopping_data[,i]), "snacks")
  shopping_data[,i][shopping_data[,i] == '5'] <- 'snacks'
}
#=====================================================================================

#defining vector and creating a shopping_seq object
shopping_data.labels<-c("general","milk","paper and health products", "snacks and drinks","vegetables and fruits")
shopping_data.scode<-c("GN","MI","PH","SN&D","VEG&FR")
seq<-seqdef(shopping_data,2:1003,states = shopping_data.scode, labels = shopping_data.labels, xtstep = 5)


#Index plot for first 10 shopping_datas
dev.new(15,15)
s<-seqiplot(seq, withlegend = F, title = "Index plot (10 first shopping_datas)",border = NA, cex.axis=0.6)
#plotting the legends- colourcode
seqlegend(seq)

#checking the alphabets (the cluster names)
alphabet(seq)

#state distribution plot
dev.new(15,15)
seqdplot(seq,border=NA,withlegend=F)

#most frequent sequences 
#seqfplot(seq,withlegend=F,border=NA)
##the graph shows that almost all the sequences are different

#mean time spent in each state for complete dataset
seqmtplot(seq,group = cards$gender,title="Mean time",withlegend=F,cex.axis=0.6)
dev.new(15,15)
seqmtplot(seq,group = cards$hh_size,title="Mean time",withlegend=F,cex.axis=0.6,las=1)

#mean time spent in each state in a sequence
seqistatd(seq[1:10,])

#calculating the entropy
entropy<-seqient(seq)
head(entropy)
dev.new(10,10)
hist(entropy, col = "cyan", main = NULL, xlab = "Entropy")
#sequence with maximum entropy
which(entropy==max(entropy))
seqistatd(seq[211,])
#sequence with minimum entropy
which(entropy==min(entropy))
seqistatd(seq[44,])

#add entropy variable to cards
cards$entropy<-entropy

#analysing the sequences near min,median & maximum entropies
entropy.quant <- quantile(cards$entropy, c(0, 0.1, 0.45, 0.55,0.9, 1))
entropy.group <- cut(cards$entropy, entropy.quant, labels = c("Min","q10-45", "Median", "q55-90", "Max"), include.lowest = T)
table(entropy.group)
entropy.group <- factor(entropy.group, levels = c("Min", "Median","Max"))
dev.new(10,10)
seqfplot(seq, group = entropy.group, pbarw = TRUE, border=NA)

#sequence entropy with categorical variables
#hh_size, age, euros,days_per_transactions
summary(cards$hh_size)
boxplot(entropy ~ hh_size, data = cards, xlab = "Household Size",ylab = "Sequences entropy", col = "cyan")
summary(cards$euros)
cards <- data.frame(cards, euros1 = cut(cards$euros, c(800,4800,8800,12800,16800,20800), 
                                        label = c("800-4800", "4800-8800","8800-12800", "12800-16800", "16800-20800"), include.lowest = TRUE))
table(cards$euros1)
boxplot(entropy ~ euros1, data = cards, xlab = "Euros Spent",ylab = "Sequences entropy", col = "cyan",cex.axis=0.6, las=2)
cards <- data.frame(cards, age1 = cut(cards$age, c(20,30,40,50,60,70,80,90), 
                                      label = c("20-30","30-40","40-50","50-60","60-70","70-80","80-90"), include.lowest = TRUE))

cards <- data.frame(cards, age1 = cut(cards$age, c(20,30,40,50,60,70,80,90), 
                                      label = c("20-30","30-40","40-50","50-60","60-70","70-80","80-90"), include.lowest = TRUE))
table(cards$age1)
boxplot(entropy ~ age1, data = cards, xlab = "Age",ylab = "Sequences entropy", col = "cyan",cex.axis=0.6, las=2)
cards <- data.frame(cards, dpt1 = cut(cards$days_per_txn, c(1,2,3,4,5,6,7,8,9), 
                                      label = c("1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9"), include.lowest = TRUE))
table(cards$dpt1)
boxplot(entropy ~ dpt1, data = cards, xlab = "Days per Transacations",ylab = "Sequences entropy", col = "cyan",cex.axis=0.6, las=2)

rm(entropy,i,n,s,entropy.group,entropy.quant,shopping_data.labels,shopping_data.scode)
cards$age1.1<-NULL
