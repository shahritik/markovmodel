##
#Sequence Analysis
##

#importing the data
library(TraMineR)
data(mvad)

#finding unique values in job sequence 
df2 <- as.vector(as.matrix(mvad[,17:86]))
unique(df2)

#Analysis
mvad.labels<-c("employment","further education","higher education","joblessness",
               "school","training")
mvad.scode<-c("EM","FE","HE","JL","SC","TR")
#we need to creat the sequence for the columns 17 to 86 
mvad.seq<-seqdef(mvad,17:86,states = mvad.scode,labels = mvad.labels,xtstep = 6)

#index plot for first 10 sequences 
seqiplot(mvad.seq, withlegend = F, title = "Index plot (10 first sequences)",border = NA)
