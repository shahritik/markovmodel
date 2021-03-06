library(dplyr)
library(readr)
library(skimr)

scans<- read.csv("./scans.csv")
scans <- scans %>% 
  mutate(card = as.character(card)) %>% 
  mutate(basket = as.character(basket)) %>%
  mutate(barcode = as.character(barcode))
#checking the data types of column
str(scans)
View(scans)
two_column <- scans %>% select(basket, category)
incidence <- as.data.frame(matrix(0L,
                                  nrow(two_column %>% distinct(basket)),
                                  nrow(two_column %>% distinct(category))
)
)
categories <- two_column %>% distinct(category) %>% arrange(category)
colnames(incidence) <- categories$category
rm(categories)
baskets <- two_column %>% distinct(basket) %>% arrange(basket)
incidence <- data.frame(baskets, incidence)
rm(baskets)
incidence_matrix <- as.matrix(incidence[, 2:ncol(incidence)])
# join the indices of the incidence onto the two_column table
# and then use these rather than doing the lookup within mapply
basket_lookup <- incidence[, 1] 
basket_lookup_tb <- tibble(basket = basket_lookup,
                           basket_index = seq_along(basket_lookup))

category_lookup <- colnames(incidence_matrix)
category_lookup_tb <- tibble(category = category_lookup,
                             category_index = seq_along(category_lookup))

four_column <- full_join(two_column, basket_lookup_tb) %>% 
  full_join(category_lookup_tb)

# this function updates the incidence_matrix
# note <<- to assign to the global incidence_matrix
fill_matrix <- function(transaction_index, category_index){
  incidence_matrix[transaction_index, category_index] <<- 1L
  return(NULL)
}

# we call mapply for the "side effect" of updating the matrix, so its output goes to tmp
# apply the updating function to each row of the tibble
system.time(tmp <- mapply(fill_matrix, four_column$basket_index, four_column$category))

rm(tmp)
rm(basket_lookup)
rm(basket_lookup_tb)
rm(category_lookup)
rm(category_lookup_tb)
rm(two_column)
rm(four_column)
rm(fill_matrix)

# turn things back to a tibble
incidence <- bind_cols(transaction = incidence[,1],
                       data.frame(incidence_matrix))

rm(incidence_matrix)

write_delim(incidence, "./incidence.txt", delim = ";", col_names = T) 
write_csv(incidence, "./incidence.csv")


rm(incidence)
rm(scans)