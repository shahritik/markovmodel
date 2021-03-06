library(seqHMM)
library(HMM)
library(depmixS4)
library(HiddenMarkov)


#plotting the sequence trajectories
ssplot(seq, border= NA)

#buliding the hmm model
sc_initmod_random <- build_hmm(observations = seq, n_states = 5)

#defining the initial, transition & emission matrix
sc_init<-c(0.2,0.195,0.197,0.203,0.205)
sc_trans<-sc_initmod_random$transition_probs
sc_emiss <- matrix(NA, nrow = 5, ncol = 5)

sc_emiss[1,] <- seqstatf(seq[, 1:200])[, 2] + 0.1
sc_emiss[2,] <- seqstatf(seq[, 201:400])[, 2] + 0.1
sc_emiss[3,] <- seqstatf(seq[, 401:600])[, 2] + 0.1
sc_emiss[4,] <- seqstatf(seq[, 601:800])[, 2] + 0.1
sc_emiss[5,] <- seqstatf(seq[, 801:1001])[, 2] + 0.1
sc_emiss <- sc_emiss / rowSums(sc_emiss)

rownames(sc_trans) <- colnames(sc_trans) <- rownames(sc_emiss) <-
  paste("State", 1:5)
colnames(sc_emiss) <- attr(seq, "labels")

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
train[,2]<-c(state=NA)
train[,3]<-c(state=NA)
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
