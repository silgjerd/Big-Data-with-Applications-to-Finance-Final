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
library(Ckmeans.1d.dp)
library(tidymodels)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation
source("modelfuncs.R")

# Params
threshold <- 0.5
resample <- "under"
current_model <- "xgb"

# Import data
train <- getTrainingData(resample, current_model) #lr: logistic regression and random forest, xg: xgboost. "no" for original dataset, "under" for undersampling, "over", for oversampling
dmtrain <- toMatrix(train)

test <- read_csv("data_test_xg.csv")
test$default <- as.factor(test$default)

# Data formats (for xgb)
xgtrain <- xgb.DMatrix(as.matrix(train %>% select(-default)), label = as.numeric(as.character(train$default)))
xgtest <- xgb.DMatrix(as.matrix(test %>% select(-default)), label = as.numeric(as.character(test$default)))

# ===================================================================



# Modelling =========================================================

# XGBoost

for (resample in c("under")) {

train <- getTrainingData(resample, current_model)

for (nrounds in c(100,400)) {
for (mgamma in c(0,5)) {
for (max_depth in c(5,10,12)) {
for (eta in c(0.02,0.05,0.07,0.1)) {
for (min_child_weight in c(3,5)) {
for (subsample in c(0.3,0.5)) {
for (colsample_bytree in c(0.5,1)) {

time.start <- Sys.time()

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  gamma = mgamma,
  max_depth = max_depth,
  eta = eta,
  min_child_weight = min_child_weight,
  subsample = subsample,
  colsample_bytree = colsample_bytree,
  tree_method = "hist",
  early_stopping_rounds = 10
)

xgmodel <- xgb.train(
  params = params,
  data = xgtrain,
  nrounds = nrounds,
  watchlist = list(val = xgtest, train = xgtrain),
  eval_metric = "auc",
  verbose = TRUE
)


xgbprobs <- predict(xgmodel, xgtest)
evaluateAndWrite(xgbprobs, current_model, "results_xgb.csv")
evaluateAndWrite(xgbprobs, current_model, "results_all.csv")

}}}}}}}}




# Current best model +++++++++++++++++++++++++++++
resample <- "under"
train <- getTrainingData(resample, current_model)
xgtrain <- xgb.DMatrix(as.matrix(train %>% select(-default)), label = as.numeric(as.character(train$default)))

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  gamma = 0,
  max_depth = 10,
  eta = 0.02,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5,
  tree_method = "hist",
  early_stopping_rounds = 10
)

mxgb <- xgb.train(
  params = params,
  data = xgtrain,
  nrounds = 400,
  watchlist = list(val = xgtest, train = xgtrain),
  eval_metric = "auc",
  verbose = TRUE
)

xgbprobs <- predict(mxgb, xgtest)
write_rds(mxgb, path = "models/xgb_best.rds")


# Model assessment
mxgb <- read_rds("models/xgb_best.rds")
xgbprobs <- predict(mxgb, xgtest)
evaluateAndWrite(xgbprobs, current_model, "results_best.csv")

plotAUC(xgbprobs)

importance <- xgb.importance(model = mxgb) #feature importance plot
xgb.ggplot.importance(importance, top_n = 15) +
  theme_light() +
  labs(title = "XGBoost Feature Importance")



pred <- if_else(xgbprobs >= threshold, 1, 0) #model predictions using threshold
conm <- confusionMatrix(as.factor(pred), test$default, positive = "1") #confusion matrix
mroc <- roc(test$default, xgbprobs) #roc object
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






writeProbs(current_model, xgbprobs, "probs.csv")
# ===================================================================

