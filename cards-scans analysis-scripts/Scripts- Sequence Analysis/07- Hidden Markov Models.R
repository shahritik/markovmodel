library(seqHMM)
library(HMM)
library(depmixS4)
library(HiddenMarkov)


#plotting the sequence trajectories
ssplot(seq, border= NA)

#defining the initial, transition & emission matrix
sc_init<-c(0.9, 0.06, 0.02, 0.01, 0.01)
sc_trans<-matrix(c(0.80, 0.10, 0.05, 0.03, 0.02, 0.02, 0.80, 0.10,
                   0.05, 0.03, 0.02, 0.03, 0.80, 0.10, 0.05, 0.02, 0.03, 0.05, 0.80, 0.10,
                   0.02, 0.03, 0.05, 0.05, 0.85), nrow = 5, ncol = 5, byrow = TRUE)
sc_emiss <- matrix(NA, nrow = 5, ncol = 5)

#we expect that the shopping sequences are somehow related to entropy
#constructing starting values by observed observable state frequencies as per entropy

cards <- data.frame(cards, entropy_range = cut(cards$entropy, c(0.60,0.68,0.76,0.84,0.92,1), 
                                        label = c("0.60-0.68", "0.68-0.76","0.76-0.84", "0.84-0.92", "0.92-1"), include.lowest = TRUE))

train_data<-seq
train_data$entropy_range<-cards$entropy_range
seq1<-train_data[(train_data$entropy_range=="0.60-0.68"),]
seq2<-train_data[(train_data$entropy_range=="0.68-0.76"),]
seq3<-train_data[(train_data$entropy_range=="0.76-0.84"),]
seq4<-train_data[(train_data$entropy_range=="0.84-0.92"),]
seq5<-train_data[(train_data$entropy_range=="0.92-1"),]

seq1$entropy_range<-NULL
seq2$entropy_range<-NULL
seq3$entropy_range<-NULL
seq4$entropy_range<-NULL
seq5$entropy_range<-NULL

sc_emiss[1,] <- seqstatf(seq1)[, 2] + 0.1
sc_emiss[2,] <- seqstatf(seq2)[, 2] + 0.1
sc_emiss[3,] <- seqstatf(seq3)[, 2] + 0.1
sc_emiss[4,] <- seqstatf(seq4)[, 2] + 0.1
sc_emiss[5,] <- seqstatf(seq5)[, 2] + 0.1
sc_emiss <- sc_emiss / rowSums(sc_emiss)

rownames(sc_trans) <- colnames(sc_trans) <- rownames(sc_emiss) <-
  paste("State", 1:5)
colnames(sc_emiss) <- attr(seq, "labels")

rm(seq1,seq2,seq3,seq4,seq5)

#printing the transition and emission matrix
sc_trans
round(sc_emiss, 3)

#buliding the new hmm model
sc_initmod <- build_hmm(observations = seq, initial_probs = sc_init,
                        transition_probs = sc_trans, emission_probs = sc_emiss)
#fitting the hmm model
sc_fit <- fit_model(sc_initmod)

#plot the states and transition probabilities
plot(sc_fit$model)

#generating the last observable state from the model to check the prediction efficiency
train<-as.data.frame(seq[,1002])
train[,2:3]<-c(state=NA)
training<-seq
colnames(train)<-c("prev_obs","prev_state","next_obs")
thetransitionmatrix<-sc_fit[["model"]][["transition_probs"]]
theemissionmatrix<-sc_fit[["model"]][["emission_probs"]]

#=============================================================================

#Viterbi Algorithm for finding the most probable state for 336 Sequences
viterbi<-function(sequence,transitionmatrix,emissionmatrix,k,train)
{
  states<-rownames(emissionmatrix)
  v<-makeViterbimat(sequence,transitionmatrix,emissionmatrix)
  mostprobablestatepath <- apply(v, 1, function(x) which.max(x))
  prevcategory <- sequence[1,1]
  prevmostprobablestate <- mostprobablestatepath[1]
  prevmostprobablestatename <- states[prevmostprobablestate]
  startpos <- 1
  for (i in 2:length(sequence))
  {
    category <- sequence[1,i]
    mostprobablestate <- mostprobablestatepath[i]
    mostprobablestatename <- states[mostprobablestate]
    if (mostprobablestatename != prevmostprobablestatename)
    {
      #print(paste("Positions",startpos,"-",(i-1), "Most probable state = ", prevmostprobablestatename))
      startpos <- i
    }
    prevcategory <- category
    prevmostprobablestatename <- mostprobablestatename
  }
  #print(paste("Positions",startpos,"-",i, "Most probable state = ", prevmostprobablestatename))
  train[k,2]<-prevmostprobablestatename
  assign('train',train,envir=.GlobalEnv)
}

#function for making the viterbi matrix
makeViterbimat <- function(sequence, transitionmatrix, emissionmatrix)
{
  numstates <- dim(transitionmatrix)[1]
  v <- matrix(NA, nrow = length(sequence), ncol = dim(transitionmatrix)[1])
  v[1, ] <- 0
  v[1,1] <- 1
  for (i in 2:length(sequence))
  {
    for (l in 1:numstates) 
    {
      statelprobcategoryi <- emissionmatrix[l,sequence[1,i]]
      v[i,l] <-  statelprobcategoryi * max(v[(i-1),] * transitionmatrix[,l])
    }
  }
  return(v)
}

#calling the viterbi function
for (k in 1:nrow(seq)) {
  myseq<-as.data.frame(training[k,])
  viterbi(myseq, thetransitionmatrix, theemissionmatrix,k,train)
}

#=============================================================================

#predicting the last observation based on previous observation and previous state
generatelastobservation <- function(transitionmatrix, emissionmatrix, train, i)
{
  states<-rownames(emissionmatrix)
  categories<-colnames(emissionmatrix)
  prev_state<-train$prev_state[i]
  stateprobs<-transitionmatrix[prev_state,]
  state<-sample(states, 1, rep=TRUE, prob=stateprobs)
  probabilities <- emissionmatrix[state,]
  category<- sample(categories, 1, rep=TRUE, prob=probabilities)
  train[i,3]<-category
  assign('train',train,envir=.GlobalEnv)
}

theemissionmatrix<-sc_fit[["model"]][["emission_probs"]]
thetransitionmatrix<-sc_fit[["model"]][["transition_probs"]]
for (i in 1:nrow(seq)) {
  generatelastobservation(thetransitionmatrix, theemissionmatrix,train, i)
}

#==============================================================================

#markov_success
training<-as.data.table(train[,-2])
training[, Markov_Success := (prev_obs == next_obs)]

markov_accuracy <-
  (100 * training[Markov_Success == TRUE, .N]) / training[, .N]
cat("Markov Accuracy:", round(markov_accuracy , 4), "%\n")

#==============================================================================

#plotting hmm model for each cluster and predicting the accuracy
num_clusters<-length(unique(cards$c5))
seq_data<-seq
weighted_mean=0

for (i in 1:5) {
  #making a dataframe for each cluster
  cards_cluster<-cards[(cards$c5==i),]
  seq_data$c5<-cards$c5
  seq_cluster<-seq_data[(seq_data$c5==i),]
  seq_cluster$c5<-NULL
  
  sc_init_cluster<-c(0.9, 0.06, 0.02, 0.01,0.01)
  sc_trans_cluster<-matrix(c(0.80, 0.10, 0.05, 0.03, 0.02, 0.02, 0.80, 0.10,
                     0.05, 0.03, 0.02, 0.03, 0.80, 0.10, 0.05, 0.02, 0.03, 0.05, 0.80, 0.10,
                     0.02, 0.03, 0.05, 0.05, 0.85), nrow = 5, ncol = 5, byrow = TRUE)
  sc_emiss_cluster <- matrix(NA, nrow = 5, ncol = 5)
  
  train_data_cluster<-seq_cluster
  train_data_cluster$entropy_range<-cards_cluster$entropy_range
  seq1<-train_data_cluster[(train_data_cluster$entropy_range=="0.60-0.68"),]
  seq2<-train_data_cluster[(train_data_cluster$entropy_range=="0.68-0.76"),]
  seq3<-train_data_cluster[(train_data_cluster$entropy_range=="0.76-0.84"),]
  seq4<-train_data_cluster[(train_data_cluster$entropy_range=="0.84-0.92"),]
  seq5<-train_data_cluster[(train_data_cluster$entropy_range=="0.92-1"),]
  
  seq1$entropy_range<-NULL
  seq2$entropy_range<-NULL
  seq3$entropy_range<-NULL
  seq4$entropy_range<-NULL
  seq5$entropy_range<-NULL
  
  sc_emiss_cluster[1,] <- seqstatf(seq1)[, 2] + 0.1
  sc_emiss_cluster[2,] <- seqstatf(seq2)[, 2] + 0.1
  sc_emiss_cluster[3,] <- seqstatf(seq3)[, 2] + 0.1
  sc_emiss_cluster[4,] <- seqstatf(seq4)[, 2] + 0.1
  sc_emiss_cluster[5,] <- seqstatf(seq5)[, 2] + 0.1
  sc_emiss_cluster <- sc_emiss_cluster / rowSums(sc_emiss_cluster)
  
  rm(seq1,seq2,seq3,seq4,seq5)
  
  sc_emiss_cluster<-as.data.frame(sc_emiss_cluster)
  
  for (l in 1:5) {
    if(rowSums(is.na(sc_emiss_cluster[l,])) == ncol(sc_emiss_cluster[l,]))
    {
      sc_emiss_cluster[l,]=0.2
    }
  }
  
  sc_emiss_cluster<-as.matrix(sc_emiss_cluster)
  
  rownames(sc_trans_cluster) <- colnames(sc_trans_cluster) <- rownames(sc_emiss_cluster) <-
    paste("State", 1:5)
  colnames(sc_emiss_cluster) <- attr(seq, "labels")
  
  #buliding the new hmm model
  sc_initmod_cluster <- build_hmm(observations = seq_cluster, initial_probs = sc_init_cluster,
                          transition_probs = sc_trans_cluster, emission_probs = sc_emiss_cluster)
  #fitting the hmm model
  sc_fit_cluster <- fit_model(sc_initmod_cluster)
  
  train_cluster<-as.data.frame(seq_cluster[,1002])
  train_cluster[,2:3]<-c(state=NA)
  training_cluster<-seq_cluster
  colnames(train_cluster)<-c("prev_obs","prev_state","next_obs")
  thetransitionmatrix<-sc_fit_cluster[["model"]][["transition_probs"]]
  theemissionmatrix<-sc_fit_cluster[["model"]][["emission_probs"]]
  
  #=============================================================================
  
  #Viterbi Algorithm for finding the most probable state for 336 Sequences
  viterbi<-function(sequence,transitionmatrix,emissionmatrix,k,train_cluster)
  {
    states<-rownames(emissionmatrix)
    v<-makeViterbimat(sequence,transitionmatrix,emissionmatrix)
    mostprobablestatepath <- apply(v, 1, function(x) which.max(x))
    prevcategory <- sequence[1,1]
    prevmostprobablestate <- mostprobablestatepath[1]
    prevmostprobablestatename <- states[prevmostprobablestate]
    startpos <- 1
    for (i in 2:length(sequence))
    {
      category <- sequence[1,i]
      mostprobablestate <- mostprobablestatepath[i]
      mostprobablestatename <- states[mostprobablestate]
      if (mostprobablestatename != prevmostprobablestatename)
      {
        #print(paste("Positions",startpos,"-",(i-1), "Most probable state = ", prevmostprobablestatename))
        startpos <- i
      }
      prevcategory <- category
      prevmostprobablestatename <- mostprobablestatename
    }
    #print(paste("Positions",startpos,"-",i, "Most probable state = ", prevmostprobablestatename))
    train_cluster[k,2]<-prevmostprobablestatename
    assign('train_cluster',train_cluster,envir=.GlobalEnv)
  }
  
  #function for making the viterbi matrix
  makeViterbimat <- function(sequence, transitionmatrix, emissionmatrix)
  {
    numstates <- dim(transitionmatrix)[1]
    v <- matrix(NA, nrow = length(sequence), ncol = dim(transitionmatrix)[1])
    v[1, ] <- 0
    v[1,1] <- 1
    for (i in 2:length(sequence))
    {
      for (l in 1:numstates) 
      {
        statelprobcategoryi <- emissionmatrix[l,sequence[1,i]]
        v[i,l] <-  statelprobcategoryi * max(v[(i-1),] * transitionmatrix[,l])
      }
    }
    return(v)
  }
  
  #calling the viterbi function
  for (k in 1:nrow(seq_cluster)) {
    myseq<-as.data.frame(training_cluster[k,])
    viterbi(myseq, thetransitionmatrix, theemissionmatrix,k,train_cluster)
  }
  
  #=============================================================================
  
  #predicting the last observation based on previous observation and previous state
  generatelastobservation <- function(transitionmatrix, emissionmatrix, train_cluster, i)
  {
    states<-rownames(emissionmatrix)
    categories<-colnames(emissionmatrix)
    prev_state<-train_cluster$prev_state[i]
    stateprobs<-transitionmatrix[prev_state,]
    state<-sample(states, 1, rep=TRUE, prob=stateprobs)
    probabilities <- emissionmatrix[state,]
    category<- sample(categories, 1, rep=TRUE, prob=probabilities)
    train_cluster[i,3]<-category
    assign('train_cluster',train_cluster,envir=.GlobalEnv)
  }
  
  theemissionmatrix<-sc_fit_cluster[["model"]][["emission_probs"]]
  thetransitionmatrix<-sc_fit_cluster[["model"]][["transition_probs"]]
  for (i in 1:nrow(seq_cluster)) {
    generatelastobservation(thetransitionmatrix, theemissionmatrix,train_cluster, i)
  }
  
  #==============================================================================
  
  #markov_success
  training_cluster<-as.data.table(train_cluster[,-2])
  training_cluster[, Markov_Success := (prev_obs == next_obs)]
  
  markov_accuracy_cluster <-
    (100 * training_cluster[Markov_Success == TRUE, .N]) / training_cluster[, .N]
  cat("Markov Accuracy:", round(markov_accuracy_cluster , 4), "%\n")
  
  weighted_mean<-weighted_mean + (markov_accuracy_cluster*nrow(seq_cluster))
}

cluster_accuracy<-weighted_mean/336
cluster_accuracy