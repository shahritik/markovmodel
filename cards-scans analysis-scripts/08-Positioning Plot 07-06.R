## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# After you have concluded with the most stable 
# solution (that is, seed number) for each k from 05. script
# you use this info to make the positioning plot
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
incidence <- read_delim("./Data/incidence.txt", delim = ";", col_names = T)
incidence_mat <- as.matrix(incidence[, -1])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# manually input k and best seed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

k <- 5
best_seed <- 313
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

col <- flxColors(1:6)
col[c(1,3)] <- flxColors(1:4, 'light')[c(1,3)]
#par(mar=rep(0,4))

plot(cli, project = incidence_pca, which = 1:2, col = col,
     asp = T, points = F, hull.args = list(density = 2), axes = T)

projAxes(incidence_pca, minradius = .1, which = 2:3, lwd = 2, col='darkblue')

# print to disk
png(file = paste0("./Segmentation, Stability and Profile/Positioning Plot for 500 Iterations/", file_name, "_p2.png"), 
    width = 480, height = 480, units = "mm", res = 500)
plot(cli, project = incidence_pca, which = 1:2, col = col,
     asp = T, points = F, hull.args = list(density = 2), axes = T)
projAxes(incidence_pca, minradius = .1, which = 2:3, lwd = 2, col='darkblue')
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove objects from the environment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(fc_cont, fc_family)
rm(k, best_seed, file_name)
rm(cli)
rm(col)
rm(incidence_pca)
rm(incidence, incidence_mat)
