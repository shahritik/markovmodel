#Clustering the Sequences based on similarity
#using different methods from the TraMineR package
#categorizing the overall state distribution and transition rates between the states
library(TraMineR)
library(cluster)
library(lattice)
library(ggplot2)
library(dplyr)

#mean time spent in each state
seqmeant(seq)
#individual time spent in each state for 336 sequences 
seqistatd(seq)
#mean time spent by male customers in each category 
seqmeant(seq[cards$gender=="Male",])
#mean time spent by female customers in each category
seqmeant(seq[cards$gender=="Female",])

#transition rate between couple of states
trate<-seqtrate(seq)
round(trate,2)

#transversal state distribution
seqstatd(seq[,1:7])

#state distribution plot
seqdplot(seq, group = cards$hh_size>3, border = NA, with.legend=F, cex.axis=0.6)

#modal state distribution
seqmsplot(seq, border=NA, with.legend=F, cex.axis=0.5)

#plotting the entropies with time step
seqHtplot(seq,group = cards$days_per_txn>3, border= NA, with.legend=F, cex.axis=0.6)

#number of transitions in each sequence
seqtransn(seq)

##measuring sequence dissimilarity
#measuring the substitution cost by transition rates
scost<-seqsubm(seq,method="TRATE")
round(scost,3)
ccost <- seqsubm(seq, method = "CONSTANT", cval = 2)

#pair wise distance matrix using optimal matching method and indel cost 1  
#seq.om_constant<-seqdist(seq,method = "OM", sm=ccost)
seq.om<-seqdist(seq,method = "OM",indel = 1,sm=scost)
#maximum distance between 2 sequences 
max(seq.om)

#measuring sequence dissimilarity and clustering
#medoid method i.e. locating the most central object 
medoid<-seqrep(seq,dist.matrix = seq.om, criterion = "dist", nrep = 1)
print(medoid, format="SPS")

#plotting more than one representative
seqrplot(seq,group = cards$gender,dist.matrix=seq.om,border=NA)

#clustering sequences
clusterward<-agnes(seq.om,diss=TRUE,method = "ward")
plot(clusterward,which.plots=2,labels=FALSE)
#dividing the sequences in 5 clusters
#each cluster has their own representatives
seq.c5<-cutree(clusterward,k=5)
c5.lab<-factor(seq.c5,labels = paste("Cluster",1:5))
dev.new(15,15)
#seqrplot(seq,group = c5.lab,dist.matrix=seq.om, trep=0.35,border=NA, with.legend=F)
seqfplot(seq,group = c5.lab, border= NA)
summary(c5.lab)

#mb5<-(c5.lab=="Cluster 5")
#glm.c5<-glm(mb5~gender + hh_size, data = cards)

rm(trate,clusterward,medoid,scost,ccost)
