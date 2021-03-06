## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script takes the data (txt files) for the stability
# plots and produces these plots
# We will need to run it 9 times (k = 3 to 8)
# it is quite a lot of commands so I did not 
# include it in a for loop
# you will need to run it manual 6 times by reading the 
# 6 files manually (I included a note at the point that you will need to 
# do that)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# libraries ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)

theme_set(theme_bw())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in and set up input for plotting and clustering (matrix object)
incidence <- read_delim("./Data/incidence-1.txt", delim = ";", col_names = T)
incidence_mat <- as.matrix(incidence[, -1])

# read in stability runs # HERE (see instructions above)!!!!!
# k = 3, 4, 5, ... 
cli_trys <- read_delim("./Data/k_cltrys_datak=3_num_trys=500.txt", delim = ";", col_names = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# most stable solution (best seed) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get a tidy tibble with cluster sizes
cli_sizes <- cli_trys %>%
  dplyr::select(k, seed, clust_num, clust_rank, size) %>%
  filter(clust_rank <= 2) %>%
  mutate(clust_label = paste0("size_", clust_rank), in_order = clust_num == clust_rank) %>%
  dplyr::select(-clust_rank, -clust_num) %>%
  spread(key = clust_label, value = size) %>%
  group_by(k, seed) %>%
  summarize(in_order = all(in_order),
            size_1 = min(size_1, na.rm = TRUE),
            size_2 = min(size_2, na.rm = TRUE)) %>% 
  ungroup()

# get location of peak numerically with MASS::kde2d
s2d <- with(cli_sizes, kde2d(size_1, size_2, n = 100))
s2d_peak <- which(s2d$z == max(s2d$z))
size_1_peak_at <- round(s2d$x[s2d_peak %% 100], 0)
size_2_peak_at <- round(s2d$y[s2d_peak %/% 100], 0)

# from which we can plot
xend <- max(cli_sizes$size_1) - 3000
yend <- max(cli_sizes$size_2) - 3000

p3 <- ggplot(cli_sizes, aes(size_1, size_2)) +
  stat_density2d(aes(fill = ..level..),  contour=TRUE, geom="polygon") +
  scale_fill_gradient(low="powderblue", high="red") +
  geom_density2d(colour = "black", size=0.2) +
  #scale_fill_continuous(low = "powderblue", high = "red") +
  annotate("segment", x = size_1_peak_at + 100 , y = size_2_peak_at + 100,
           xend = xend, yend = yend - 200, size = 1) +
  annotate("text", xend, yend, label = paste0("(", size_1_peak_at, ", ", size_2_peak_at, ")"), vjust = 0) +
  xlab("Cluster 1 Size") +
  ylab("Cluster 2 Size") +
  #xlim(min(cli_sizes$size_1) - 2500, max(cli_sizes$size_1) + 2500) +
  #ylim(min(cli_sizes$size_2) - 5000, max(cli_sizes$size_2) + 5000) +
  geom_point(alpha = 0.25, size = 2) +
  ggtitle(NULL) +
  theme(legend.position = "none")

# get distance of each solution's first & second cluster counts to the peak we found above
cli_best <- cli_sizes %>%
  filter(in_order) %>%    ## just look at solutions with clusters in decending sizes
  mutate(distance = sqrt((size_1 - size_1_peak_at) ^ 2 + (size_2 - size_2_peak_at) ^ 2)) %>%
  arrange(distance)

# get the seed that produces the most stable solution (best) 
best_seed <- as.integer(cli_best[1, 2])

# get the k
num_clusters <- as.integer(cli_best[1, 1])

file_name <- paste0("k=", num_clusters, "_seed=", best_seed)

## plot size of cluster 2 by size of cluster 1 for k and num_trys
# make sure you use the correct directory in your machine 
p3; ggsave(paste0("./Segmentation, Stability and Profile/Stability Figures with 500 Iterations/", file_name, "_stab.png"), width = 15, height = 15, dpi = 500)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# remove objects from the environment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(cli_sizes, cli_trys, cli_best) # kcca results
rm(s2d, s2d_peak, size_1_peak_at, size_2_peak_at) # MASS objects
rm(xend, yend, p3) # object from p3
rm(best_seed, file_name, num_clusters)
rm(incidence, incidence_mat)

