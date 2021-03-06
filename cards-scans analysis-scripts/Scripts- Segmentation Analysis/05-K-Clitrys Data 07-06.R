## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script takes the incidence table and data and produces the
# data that we will later use to product the stability plots
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(flexclust)
library(MASS)


# data
# make sure you use the correct directory in your machine 
incidence<-read.csv("./Data/incidence.csv")
incidence_mat <- as.matrix(incidence[, -1])

# global settings (do not change that)
fc_cont <- new("flexclustControl") ## flexclustControl object holds "hyperparameters"
fc_cont@tolerance <- 0.1 ## kcca only uses if classify == "weighted"
fc_cont@iter.max <- 30
fc_cont@verbose <- 0
fc_family <- "ejaccard" ## distance metric, Jaccard distance w/ centroid means

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the for loop below runs 6 times for cluster 
# number 3, 4, 5, 6, up to 8 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (num_clusters in 3:8) {
  
  # local
  fc_seed <- 123
  num_trys <- 500
  cli_trys <- NULL
  
  # invoke kcca a few times (num_trys) and keep run information (cli_trys)
  for (itry in 1:num_trys) { # it runs 500 "cluster runs"
    
    # set seeds
    fc_seed <- fc_seed + 1 # incread seed by one in each run
    set.seed(fc_seed)
    
    # invoke kcca
    cli <- kcca(incidence_mat, k = num_clusters, save.data = TRUE,
                control = fc_cont, family = kccaFamily(fc_family))
    
    # get info from kcca object
    cli_info <- cli@clusinfo %>%
      mutate(clust_num = row_number(),
             clust_rank = rank(desc(size), ties.method = "first")) %>%
      arrange(clust_rank) %>%
      dplyr::select(c(6, 5, 1:4))
    
    # combine k and seed parameters with info from the kcca object
    cli_try <- cbind(data.frame(k = num_clusters, seed = fc_seed), cli_info)
    
    # combine k, seed, and kcca info for all trys
    cli_trys <- rbind(cli_trys, cli_try)
    
  }
  
  # make a tbl print it 
  cli_trys <- as.tbl(cli_trys)
  
  #  print tbl
  file_name <- paste0("k=", num_clusters, "_num_trys=", num_trys)
  # make sure you use the correct directory in your machine 
  write_delim(cli_trys, paste0("./Data/k_cltrys_data", file_name, ".txt"), delim = ";", col_names = T)
  write_csv(cli_trys,paste0("./Data/k_cltrys_data",file_name,".csv"))
  
}
