## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script produces the hierarchical clustering 
# of the clustering variables (in our case categories)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# libraries ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(ape)
library(MVA)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make sure you use the correct directory in your machine 
incidence <- read_delim("./Data/incidence.txt", delim = ";", col_names = T)
#incidence1<-incidence
#drops <- c("transaction")
#incidence2<-incidence1[ , !(names(incidence1) %in% drops)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# hierarchical clustering of variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat_hier <- hclust(dist(t(incidence[, -1])), "ward")

#par(mar = c(0, 0, 0, 15)) # sets the margin sizes (bottom, left, top, and right)

clus6 = cutree(cat_hier, 6)
colors <- c("magenta", "dodgerblue", "firebrick", "forestgreen", "goldenrod2", "black")

plot(as.phylo(cat_hier), type = "phylogram", tip.color = colors[clus6],
     label.offset = 4, cex = 0.5)


#dev.off() # cancels par

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# empty environment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(incidence)
