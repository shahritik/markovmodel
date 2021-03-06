#Sequences are divided into 8 clusters
#Differences in clusters are measured using histograms and Tukeys honest significant differences

#adding the quantity of each state in a sequence to cards dataset================

cards$c5<-as.factor(seq.c5)
#df2<-as.vector(as.matrix(seq))
#category_names<-unique(df2)

#category<-data.frame(matrix(ncol = 7, nrow = 0))
#colnames(category)<-category_names
#category<-category[,order(names(category))]

#for (i in 1:nrow(seq)) {
#  df1<-as.data.frame(table(unlist(seq[i,])))
#  category[i,]<-df1$Freq
#}
#category[,6:7]<-NULL

#cards<-cbind(cards,category)
#=================================================================================
#df3 <- read.csv(text=paste(unlist(t(seq)), collapse=","), header=FALSE, stringsAsFactors=FALSE)
#df4<-as.data.frame(t(df3))
#write_csv(df3,paste0("./Sequence Clustering","df3",".csv"))
#write_csv(df4,paste0("./Sequence Clustering","df3_transpose",".csv"))

df4<-read.csv("./Sequence Clustering/df3_transpose.csv")

df4[,2]<-data.frame(c5=rep(c(cards$c5),each=1002))
colnames(df4)[1]<-"category"

#=================================================================================

#plotting the clusters with categorical variables
histogram( ~ euros | factor(paste("Cluster", c5)),data = cards, as.table = TRUE)
histogram( ~ age | factor(paste("Cluster", c5)),data = cards, as.table = TRUE)
histogram( ~ hh_size | factor(paste("Cluster", c5)),data = cards, as.table = TRUE)
histogram( ~ dpt1 | factor(paste("Cluster", c5)),data = cards, as.table = TRUE)
histogram( ~ entropy | factor(paste("Cluster", c5)),data = cards, as.table = TRUE)
histogram( ~ category | factor(paste("Cluster", c5)),data = df4, as.table = TRUE)

#Euros
aov1 <- aov(euros ~ c5, data = cards)
plot(TukeyHSD(aov1), las = 1, col="brown")
mtext("Pairs of segments", side = 2, line = 3)

#hh_size
aov2 <- aov(hh_size ~ c5, data = cards)
plot(TukeyHSD(aov2), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

#days per transaction
aov3 <- aov(days_per_txn ~ c5, data = cards)
plot(TukeyHSD(aov3), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

#units
aov4 <- aov(units ~ c5, data = cards)
plot(TukeyHSD(aov4), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

#age
aov5 <- aov(age ~ c5, data = cards)
plot(TukeyHSD(aov5), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

#entropy
aov6 <- aov(entropy ~ c5, data = cards)
plot(TukeyHSD(aov6), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

rm(aov1,aov2,aov3,aov4,aov5,aov6,df1,df2,i,category_names,df4,seq.c5,c5.lab)
