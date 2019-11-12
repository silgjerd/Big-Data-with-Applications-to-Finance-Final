library(stats)
library(quantmod)
library(caTools)
library(tidyverse)
library(stargazer)
library(gvlma)
library(lindia)
library(car)
library(lmtest)
library(DescTools)
library(readxl)
library(caret)
library(e1071)
library(outliers)
library(mltools)
library(onehot)
library(BBmisc)
library(scales)
library(ROCR)
library(pROC)
library(xgboost)
library(randomForest)
library(ranger)
library(DMwR)
library(corrplot)
library(tidymodels)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation
source("modelfuncs.R")

# Params
threshold <- 0.5
resample <- "under" # "no" for original dataset, "under" for undersampling, "over", for oversampling
current_model <- "ran"

# Import data
train <- getTrainingData(resample, current_model)

test <- read_csv("data_test_rf.csv")
test$default <- as.factor(test$default)

# ===================================================================



# Modelling =========================================================



# Random forest

for (resample in c("under", "over")){
  
  train <- getTrainingData(resample, current_model)
  
  for (mtrees in c(500,800,1000,1500,2000,3000)) {
    for (mmtry in c(2,5,8,10,20,30,35,39)) {
      
      time.start <- Sys.time()
      
      mran <- ranger(default ~ .,
                     data = train,
                     mtry = mmtry,
                     importance = "impurity",
                     num.trees = mtrees,
                     verbose = TRUE,
                     probability = TRUE)
      
      ranprobs <- predict(mran, test)$prediction[,2] #probabilites 
      evaluateAndWrite(ranprobs, current_model, "results_ran.csv")
      evaluateAndWrite(ranprobs, current_model, "results_all.csv")
      
      cat("Trees:", mtrees, ", Mtry:", mmtry, "\n") #print status
    }
  }
}



# Current best model +++++++++++++++++++++++++++++
mmtry <- 8
mtrees <- 1500

mran <- ranger(default ~ .,
               data = train,
               mtry = mmtry,
               importance = "impurity",
               num.trees = mtrees,
               verbose = TRUE,
               probability = TRUE)

ranprobs <- predict(mran, test)$prediction[,2]
write_rds(mran, path = "models/ran_best.rds")



# Model assessment
mran <- read_rds("models/ran_best.rds")
ranprobs <- predict(mran, test)$prediction[,2]
evaluateAndWrite(ranprobs, current_model, "results_best.csv")

plotAUC(ranprobs)

plot_ranger_importance(mran)


pred <- if_else(ranprobs >= threshold, 1, 0) #model predictions using threshold
conm <- confusionMatrix(as.factor(pred), test$default, positive = "1") #confusion matrix
mroc <- roc(test$default, ranprobs) #roc object
modeloutput <- tibble("nn" = conm$table[1],
                      "pn" = conm$table[2],
                      "np" = conm$table[3],
                      "pp" = conm$table[4],
                      "Sensitivity" = conm$byClass[[1]],
                      "Specificity" = conm$byClass[[2]],
                      "Accuracy" = conm$overall[[1]],
                      "Balanced_Accuracy" = conm$byClass[[11]],
                      "AUC" = auc(mroc),
                      "Threshold" = threshold,
                      "Resample" = resample,
                      "trees" = ifExistsElseNull(mtrees),
                      "mtry" = ifExistsElseNull(mmtry))
modeloutput







writeProbs(current_model, ranprobs, "probs.csv")
# ===================================================================



