## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# After you have concluded with the most stable 
# solution (that is, seed number) for each k from 05. script
# you use this info to make the seperation plot
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
# make sure you use the correct directory in your machine 
incidence <- read_delim("./Data/incidence-1.txt", delim = ";", col_names = T)
incidence_mat <- as.matrix(incidence[, -1])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# manually input k and best seed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

k <- 8
best_seed <- 390
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

# perform pca
incidence_pca <- prcomp(incidence_mat) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# seperation plot (pc1, pc2)
plot(cli, project = incidence_pca, which = 1:2, asp = F, points = F, hull.args = list(density = 5))

# seperation plot (pc1, pc2, pc3)
# pairs(cli, project = incidence_pca, which = 1:3, asp = F, points = F, hull.args = list(density = 5))

# print to disk
png(file = paste0("./Segmentation, Stability and Profile/Separation Plot for 500 Iterations/", file_name, "_p1.png"), 
    width = 380, height = 380, units = "mm", res = 500)
plot(cli, project = incidence_pca, which = 1:2, asp = F, points = F, hull.args = list(density = 5))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove objects from the environment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(fc_cont, fc_family) # kcca global parameters 
rm(k, best_seed, file_name) # kcca local parameters
rm(cli, incidence_pca) # kcca results
rm(incidence, incidence_mat)

