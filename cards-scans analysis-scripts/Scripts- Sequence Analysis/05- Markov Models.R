#hidden markov models for sequence prediction
library(seqHMM)
library(TraMineR)
library(markovchain)
library(diagram)
library(expm)
#mcFit<-markovchainFit(data = seq)

#markov<-build_hmm(observations=seq, n_states = 5)

#making the transition matrix
transition<-createSequenceMatrix(seq)

#fitting a single markov model for the complete data
myFit<-markovchainFit(data=seq,confidencelevel = .9,method = "mle")
myFit

#transition matrix with transition probabilities
alofiMc<-myFit$estimate
alofiMc

a11=alofiMc[1,1]
a12=alofiMc[1,2]
a13=alofiMc[1,3]
a14=alofiMc[1,4]
a15=alofiMc[1,5]
a21=alofiMc[2,1]
a22=alofiMc[2,2]
a23=alofiMc[2,3]
a24=alofiMc[2,4]
a25=alofiMc[2,5]
a31=alofiMc[3,1]
a32=alofiMc[3,2]
a33=alofiMc[3,3]
a34=alofiMc[3,4]
a35=alofiMc[3,5]
a41=alofiMc[4,1]
a42=alofiMc[4,2]
a43=alofiMc[4,3]
a44=alofiMc[4,4]
a45=alofiMc[4,5]
a51=alofiMc[5,1]
a52=alofiMc[5,2]
a53=alofiMc[5,3]
a54=alofiMc[5,4]
a55=alofiMc[5,5]

stateNames <- c("GN","MI","PH","SN&D","VEG&FR")

ra <- matrix(c(a11,a12,a13,a14,a15,a21,a22,a23,a24,a25,a31,a32,a33,a34,a35,a41,a42,a43,a44,a45,a51,a52,a53,a54,a55),nrow=5, byrow=TRUE)

dtmcA <- new("markovchain",transitionMatrix=ra, states=c("GN","MI","PH","SN&D","VEG&FR"), name="MarkovChain A") 

dtmcA

#predicting the next state
x1 <- matrix(c(0,1,0,0,0),nrow=1, byrow=TRUE)

#after day 1,2,3,4,5,6,7
x1 %*% ra

ra2 <- ra %^% 2
ra3 <- ra %^% 3
ra4 <- ra %^% 4
ra5 <- ra %^% 5
ra6 <- ra %^% 6
ra7 <- ra %^% 7
ra10 <- ra %^% 10
ra15 <- ra %^% 15

#probailities after day 7
round(x1%*%ra15,3)
