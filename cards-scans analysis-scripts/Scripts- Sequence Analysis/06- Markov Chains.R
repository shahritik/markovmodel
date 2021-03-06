library(markovchain)
library(ggplot2)
library(data.table)
library(stringr)
library(Rsolnp)
library(discretization)
library(tidyr)

training<-as.data.table(seq)

mk1<-markovchainFit(training)

baseline<-max(table(training[,1002]))/336

training <- unite(training, Sequence, sep = ",", remove = TRUE)
training$Last <- sub('.*,\\s*', '', training$Sequence)
training$Problem<-gsub("(.*),.*", "\\1", training$Sequence)

markov_predict <- function(my_problem, n = 3L, verbose = FALSE) {
  my_problem <- unlist(str_split(my_problem, ","))
  len <- length(my_problem)
  n <- min(len, n) # new data length
  new_data <- my_problem[(len - n + 1L):len]
  mk <- mk1
  prediction <- predict(object = mk$estimate,
                        newdata = new_data,
                        n.ahead = 1L)
  loglike <- mk$logLikelihood
  answer <-
    list("Markov_Prediction" = prediction,
         "Markov_Loglikelihood" = loglike)
  if(verbose) {
    cat(paste("Markov Prediction:", prediction, "\n"))
    cat(paste("Markov Loglikelihood:", loglike, "\n"))
  }
  return(answer)
}

markov_result <- t(sapply(training[, Problem], markov_predict))
training <- cbind(training, markov_result)
rm(markov_result)
training[, Markov_Prediction := unlist(Markov_Prediction)]
training[, Markov_Loglikelihood := unlist(Markov_Loglikelihood)]
training[, Markov_Success := (Last == Markov_Prediction)]

markov_accuracy <-
  (100 * training[Markov_Success == TRUE, .N]) / training[, .N]
cat("Markov Accuracy:", round(markov_accuracy , 4), "%\n")

#markov efficiency for the clusters
train<-as.data.table(seq)
train$c5<-cards$c5
len<-length(unique(cards$c5))
cluster_prediction=0

for (i in 1:len) {
  train_cluster<-train[(train$c5==i),]
  train_cluster$c5<-NULL
  mk2<-markovchainFit(train_cluster)
  train_cluster <- unite(train_cluster, Sequence, sep = ",", remove = TRUE)
  train_cluster$Last <- sub('.*,\\s*', '', train_cluster$Sequence)
  train_cluster$Problem<-gsub("(.*),.*", "\\1", train_cluster$Sequence)
  
  markov_predict <- function(my_problem, n = 3L, verbose = FALSE) {
    my_problem <- unlist(str_split(my_problem, ","))
    len <- length(my_problem)
    n <- min(len, n) # new data length
    new_data <- my_problem[(len - n + 1L):len]
    mk <- mk2
    prediction <- predict(object = mk$estimate,
                          newdata = new_data,
                          n.ahead = 1L)
    loglike <- mk$logLikelihood
    answer <-
      list("Markov_Prediction" = prediction,
           "Markov_Loglikelihood" = loglike)
    if(verbose) {
      cat(paste("Markov Prediction:", prediction, "\n"))
      cat(paste("Markov Loglikelihood:", loglike, "\n"))
    }
    return(answer)
  }
  
  markov_result <- t(sapply(train_cluster[, Problem], markov_predict))
  train_cluster <- cbind(train_cluster, markov_result)
  rm(markov_result)
  train_cluster[, Markov_Prediction := unlist(Markov_Prediction)]
  train_cluster[, Markov_Loglikelihood := unlist(Markov_Loglikelihood)]
  train_cluster[, Markov_Success := (Last == Markov_Prediction)]
  
  markov_accuracy <-
    (100 * train_cluster[Markov_Success == TRUE, .N]) / train_cluster[, .N]
  cat("Markov Accuracy:", round(markov_accuracy , 4), "%\n")
  cluster_prediction=cluster_prediction + (markov_accuracy*nrow(train_cluster))
}

cluster_prediction=cluster_prediction/336
cluster_prediction

rm(len,train,train_cluster,i,training,baseline)
rm(clusterward,seq.c5,cluster_prediction,markov_accuracy)

