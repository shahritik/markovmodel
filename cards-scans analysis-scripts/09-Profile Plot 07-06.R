## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# After you have concluded with the most stable 
# solution (that is, seed number) for each k from 05. script
# you use this info to make the profiles plot
# for each k
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# libraries ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(flexclust)

theme_set(theme_bw())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in and set up input for plotting and clustering (matrix object)
incidence <- read_delim("./Data/incidence-1.txt", delim = ";", col_names = T)
incidence_mat <- as.matrix(incidence[, -1])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# manually input k and best seed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

k <- 5
best_seed <- 362
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
cli <- kcca(incidence_mat, k = k, save.data = T,
            control = fc_cont, family = kccaFamily(fc_family))

# perform pca
incidence_pca <- prcomp(incidence_mat) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat_hier <- hclust(dist(t(incidence[, -1])), "ward")

barchart(cli, strip.prefix = "#", shade = T, layout = c(cli@k, 1), which = rev(cat_hier$order), 
         legend = T, ncol = 1)

par(mar = c(10, 0, 0, 0)) # sets the margin sizes (bottom, left, top, and right)
barchart(cli, strip.prefix = "#", shade = T, layout = c(cli@k, 1), which = rev(cat_hier$order))

legend("bottom", ncol = 1, horiz = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove objects from the environment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(fc_cont, fc_family)
rm(k, best_seed, file_name)
rm(cli)
rm(col)
rm(incidence_pca)
rm(incidence, incidence_mat)
