##
#Sankey Diagram for comparing the segment division and
#how observations get divided with increasing clusters 
##

library(readr)
library(plyr)
library(dplyr)
library(networkD3)
library(curl)

segments_slsa<-read_delim("./Data/Segments.txt", delim = ";", col_names = T)

segments_slsa1<-segments_slsa[-c(1:10)]

df2=list()
t=3
x=3
for (i in 1:5) {
  segments_slsa1[i+1]=segments_slsa1[i+1]+x
  df<-data.frame(segments_slsa1[i],segments_slsa1[i+1])
  colnames(df)<-c("source","target")
  df1<-df %>% group_by_at(1:2) %>% summarise(count=n())
  colnames(df1)<-c("source","target","value")
      df2[[i]]<-df1
      t=t+1
      x=x+t
    }
  df3_links=as.data.frame(do.call(rbind,df2))
  
  df3_links$source<-df3_links$source-1
  df3_links$target<-df3_links$target-1
  
  ddf3_nodes=data.frame("name" = 
                          c("1","2","3", 
                            "4","5","6","7",
                            "8","9","10","11","12","13","14",
                            "15","16","17","18","19","20","21",
                            "22","23","24","25","26","27","28","29",
                            "30","31","32","33"), stringsAsFactors = FALSE)
  
  
  sankeyNetwork(Links = df3_links, Nodes = ddf3_nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name", 
                fontSize=12, nodeWidth = 10)

