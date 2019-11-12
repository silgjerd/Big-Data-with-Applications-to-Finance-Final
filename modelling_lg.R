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
current_model <- "log"

# Import data
train <- getTrainingData(resample, current_model) #no, under, over : log, ran, xgb

test <- read_csv("data_test_lg.csv")
test$default <- as.factor(test$default)

# ===================================================================



# Modelling =========================================================


# Logistic regression

for (resample in c("no", "under", "over")) {
  
  train <- getTrainingData(resample, current_model)
  
  time.start <- Sys.time()
  
  mlog <- glm(default ~ .,
              data = train,
              family = "binomial")
  
  logprobs <- predict(mlog, test, type = "response")
  evaluateAndWrite(logprobs, current_model, "results_log.csv")
  evaluateAndWrite(logprobs, current_model, "results_all.csv")
  
  cat(resample)
}



# Current best model +++++++++++++++++++++++++++++
resample <- "under"
train <- getTrainingData(resample, current_model)

mlog <- glm(default ~ .,
            data = train,
            family = "binomial")

logprobs <- predict(mlog, test, type = "response")
write_rds(mlog, path = "models/log_best.rds")




# Model assessment
mlog <- read_rds("models/log_best.rds")
logprobs <- predict(mlog, test, type = "response")
evaluateAndWrite(logprobs, current_model, "results_best.csv")

plotAUC(logprobs)

stargazer(mlog,
          type = "text",
          keep.stat = c("adj.rsq", "rsq", "n"),
          report = "vc*stp")

dff <- varImp(mlog)
dff <- rownames_to_column(dff, "Variable")
dff %>% arrange(desc(Overall))





writeProbs(current_model, logprobs, "probs.csv")
# ===================================================================


