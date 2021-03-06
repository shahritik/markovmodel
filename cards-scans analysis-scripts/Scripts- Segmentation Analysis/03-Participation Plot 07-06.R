library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())
incidence<- read.csv("./data/incidence.csv")

incidence[, -1] %>%
  gather("category", "transaction") %>% 
  group_by(category) %>%
  summarize(pct_bought = 100.0 * sum(transaction) / n() ) %>%
  mutate(category = reorder(category, pct_bought)) %>% # makes the arrange
  arrange(desc(pct_bought)) %>% # makes the arrange
  ggplot(aes(category, pct_bought)) +
  geom_col(fill = "dodgerblue2") +
  labs(x = NULL, y = "Transactions %", title = "Participation of Categories") +
  #geom_hline(yintercept = 15,linetype = "dashed", color = "red", size = 1) +
  #geom_hline(yintercept = 40,linetype = "dashed", color = "red", size = 1) +
  coord_flip()

#percentage<-colMeans(incidence[,-1])
#percentage1<-as.data.frame(round(percentage*100,1))

# category participation alphabetical
incidence[, -1] %>%
  gather("category", "transaction") %>% 
  group_by(category) %>%
  summarize(pct_bought = 100.0 * sum(transaction) / n()) %>% 
  ggplot(aes(category, pct_bought)) +
  geom_col(fill = "dodgerblue2") +
  labs(x = NULL, y = "Transactions %", title = "Participation of Categories") +
  coord_flip()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# empty environment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(incidence)

