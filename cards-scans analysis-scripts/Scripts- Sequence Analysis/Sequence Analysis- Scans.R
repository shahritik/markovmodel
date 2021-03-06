#import library
library(TraMineR)
library(readr)
library(tidyr)
library(data.table)

##preparing the sequence dataset for 7 clusters
#=====================================================================================
scans <- read_delim("./Data/scans_nc.txt", delim = ";", col_names = T)
cards <- read_delim("./Data/cards_nc.txt", delim = ";", col_names = T)

e<-unstack(scans1, c6~card)
#converting the list into dataframe
f<-plyr::ldply(e, rbind)

#checking the number of distinct baskets
#checking the maximum and minimum length of sequence
g<-ddply(scans,~card,summarise,number_of_distinct_baskets=length(unique(basket)))
max(g$number_of_distinct_baskets)
min(g$number_of_distinct_baskets)

#merging the sequence with cards data
sequence<-cbind(cards, column = f[2:719])

#naming the segments
for (i in 13:ncol(sequence)) {
  levels(sequence[,i]) <- c(levels(sequence[,i]), "general")
  sequence[,i][sequence[,i] == '1'] <- 'general'
  levels(sequence[,i]) <- c(levels(sequence[,i]), "fruits")
  sequence[,i][sequence[,i] == '2'] <- 'fruits'
  levels(sequence[,i]) <- c(levels(sequence[,i]), "vegetables")
  sequence[,i][sequence[,i] == '3'] <- 'vegetables'
  levels(sequence[,i]) <- c(levels(sequence[,i]), "milk")
  sequence[,i][sequence[,i] == '4'] <- 'milk'
  levels(sequence[,i]) <- c(levels(sequence[,i]), "snacks")
  sequence[,i][sequence[,i] == '5'] <- 'snacks'
  levels(sequence[,i]) <- c(levels(sequence[,i]), "drinks")
  sequence[,i][sequence[,i] == '6'] <- 'drinks'
  levels(sequence[,i]) <- c(levels(sequence[,i]), "paper and health")
  sequence[,i][sequence[,i] == '7'] <- 'paper and health'
}
#=====================================================================================

#defining vector and creating a sequqence object
sequence.labels<-c("alcohol and soft drinks","fruits","general","milk","paper and health products", "snacks","vegetables")
sequence.scode<-c("DR","FR","GN","MI","PH","SN","VEG")
seq<-seqdef(sequence,13:730,states = sequence.scode, labels = sequence.labels, xtstep = 7)

#renaming the columns with basket number
names(seq)[1:718] <- paste("B", 1:718, sep="")

#Index plot for first 10 Sequences
s<-seqiplot(seq, withlegend = F, title = "Index plot (10 first sequences)",border = NA, cex.axis=0.6)
#plotting the legends- colourcode
seqlegend(seq)

#length of sequences in sequence object
seqlength(seq)

#the time spent in each state 
seqistatd(seq[1:6, ])

#the mean time spent in each state
statd <- seqistatd(seq[1:6,])
apply(statd, 2, mean)

#calculate the entropy, measure the diversity of states
#how frequent is the change
entropy<-seqient(seq)
head(entropy)
summary(entropy)

#plot histogram for entropies 
hist(entropy, col = "cyan", main = NULL, xlab = "Entropy",cex.axis=0.7,las=1, labels = TRUE, ylim = c(0,140),border = "blue", breaks = 10, xlim = c(0.75,1), ylab = "Frequency of Consumers")

#which transaction has the highest and the lowest entropy
which(entropy==max(entropy))
which(entropy==min(entropy))

#comparing the entropies with covariates
cards<-data.frame(cards,entropy)
cards_quant <- quantile(cards$Entropy, c(0, 0.1, 0.45, 0.55,0.9, 1))
cards_group <- cut(cards$Entropy, cards_quant, labels = c("Min","q10-45", "Median", "q55-90", "Max"), include.lowest = T)
table(cards_group)
cards_group <- factor(cards_group, levels = c("Min", "Median","Max"))
table(cards_group)
dev.new(15,15)
seqfplot(seq, group = cards_group, pbarw= TRUE, border = NA, xlim= c(1,380))

#sequence entropy with categorical variables
#hh_size, age, euros,days_per_transactions
summary(cards$hh_size)
boxplot(Entropy ~ hh_size, data = cards, xlab = "Household Size",ylab = "Sequences entropy", col = "cyan")
summary(cards$euros)
cards <- data.frame(cards, euros1 = cut(cards$euros, c(800,4800,8800,12800,16800,20800), 
label = c("800-4800", "4800-8800","8800-12800", "12800-16800", "16800-20800"), include.lowest = TRUE))
table(cards$euros1)
boxplot(Entropy ~ euros1, data = cards, xlab = "Euros Spent",ylab = "Sequences entropy", col = "cyan",cex.axis=0.6, las=2)
cards <- data.frame(cards, age1 = cut(cards$age, c(20,30,40,50,60,70,80,90), 
label = c("20-30","30-40","40-50","50-60","60-70","70-80","80-90"), include.lowest = TRUE))
table(cards$age1)
boxplot(Entropy ~ age1, data = cards, xlab = "Age",ylab = "Sequences entropy", col = "cyan",cex.axis=0.6, las=2)
cards <- data.frame(cards, dpt1 = cut(cards$days_per_txn, c(1,2,3,4,5,6,7,8,9), 
label = c("1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9"), include.lowest = TRUE))
table(cards$dpt1)
boxplot(Entropy ~ dpt1, data = cards, xlab = "Days per Transacations",ylab = "Sequences entropy", col = "cyan",cex.axis=0.6, las=2)

#measuring the length of longest common subsequence between 2 Sequences
seqmpos(seq[1,],seq[2,])

#removing variables
rm(e,f,g,statd,entropy,scans,cards,cards_group,cards_quant
   ,i,mapping,s,sequence.labels,sequence.scode)
