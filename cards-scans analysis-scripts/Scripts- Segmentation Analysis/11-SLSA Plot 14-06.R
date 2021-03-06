##
#SLSA plot for the segment division keeping the seed same
##

incidence <- read_delim("./Data/incidence-1.txt", delim = ";", col_names = T)
incidence_mat <- as.matrix(incidence[, -1])

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

set.seed(362)

# perform cluster analysis
cli <- stepcclust(incidence_mat, k = 3:6,
                         method = "neuralgas", nrep = 20, save.data = TRUE,
                         verbose = FALSE)
cli <- relabel(cli)

#SLSA plot
slsaplot(cli, nodecol = scans1$basket)

