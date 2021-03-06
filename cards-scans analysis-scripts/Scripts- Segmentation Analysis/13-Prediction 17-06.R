##
#Using Multinomial Regression model to predict the Segment Number
#Check which Segment Solution gives better accuracy (Segment 5 and Segment 7)
#Using various models by changing independent variables
#Taking the model having low AIC value
##

library(nnet)
library(effects)
library(car)
library(caret)
library(ROCR)
library(Metrics)
library(plyr)
library(data.table)
library(stringr)
library(parallelMap)
library(parallel)
library(mlr)
library(h2o)

#logistic regression models with different independent variables
model1 <- multinom(c6 ~ units + euros, data = scans1, trace = 0)
summary(model1)

model2 <- multinom(c6 ~ units + euros + num_of_cat, data = scans1, trace = 0)
summary(model2)

model3 <- multinom(c6 ~ units + euros + num_of_cat + day1 + age, data = scans1, trace = 0)
summary(model3)

#comparing all the 2 models
anova(model1,model2,model3,test = "Chisq")
#with p>0.05 2nd model is better than the first 

#predicting for the unseen data
#Divide the scans1 dataset into test and train
## 75% of the sample size
smp_size <- floor(0.75 * nrow(scans1))

## set the seed to make your partition reproducible
set.seed(589)
train_ind <- sample(seq_len(nrow(scans1)), size = smp_size)

train <- scans1[train_ind, ]
test <- scans1[-train_ind, ]

#model training and prediction
model_new <- multinom(c6 ~ units + euros + num_of_cat, data = train, trace = 0)
model_predict <- predict(model_new,newdata = test)

table(model_predict)

#checking the model accuracy
auc(test$c6,model_predict)

#randomforest regression model
fact_col <- colnames(train)[sapply(train,is.character)]
for(i in fact_col)
  set(train,j=i,value = factor(train[[i]]))
for(i in fact_col)
  set(test,j=i,value = factor(test[[i]]))

fact_col <- colnames(test)[sapply(train,is.character)]
for(i in fact_col)
  set(train,j=i,value = factor(train[[i]]))
for(i in fact_col)
  set(test,j=i,value = factor(test[[i]]))

traintask <- makeClassifTask(data = train,target = "c6")
testtask <- makeClassifTask(data = test,target = "c6")

bag <- makeLearner("classif.rpart",predict.type = "response")
bag.lrn <- makeBaggingWrapper(learner = bag,bw.iters = 100,bw.replace = TRUE)

rdesc <- makeResampleDesc("CV",iters=5L)

parallelStartSocket(cpus = detectCores())

r <- resample(learner = bag.lrn
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(acc)
              ,show.info = T)

#Accuracy with random forest is 29.63%
