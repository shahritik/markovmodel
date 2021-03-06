library(ggplot2)
library(markovchain)
library(data.table)
library(stringr)

rm(KAGGLE,SAMPLE_SIZE)

KAGGLE <- TRUE
SAMPLE_SIZE <- 10000

training1 <- read.csv("./train.csv", stringsAsFactors = FALSE)
training1 <- as.data.table(training1)
set.seed(pi)
training1 <- training1[sample(.N, SAMPLE_SIZE)]

training1[, Last := str_extract(Sequence, "\\-?\\d*$")]
training1[, Problem := gsub("\\,\\-?\\d*$", "\\1", Sequence)]

markov_predict <- function(my_problem, n = 3L, verbose = FALSE) {
  my_problem <- unlist(str_split(my_problem, ","))
  len <- length(my_problem)
  n <- min(len, n) # new data length
  new_data <- my_problem[(len - n + 1L):len]
  mk <- markovchainFit(my_problem)
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

markov_result <- t(sapply(training1[, Problem], markov_predict))
training1 <- cbind(training1, markov_result)
rm(markov_result)
training1[, Markov_Prediction := unlist(Markov_Prediction)]
training1[, Markov_Loglikelihood := unlist(Markov_Loglikelihood)]
training1[, Markov_Success := (Last == Markov_Prediction)]