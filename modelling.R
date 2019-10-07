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
library(DMwR)
library(corrplot)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# Import data
train <- read_csv("data_train.csv")
test <- read_csv("data_test.csv")


# EDA
train %>% group_by(default) %>% tally()
corrplot(cor(train), method="color")








xtrain <- train[,-1]
ytrain <- train[,1]
xtest <- test[,-1]
ytest <- test[,1]


# Modelling

#Logistic regression
ytrain <- as.factor(ytrain)
ytest <- as.factor(ytest)

m_logit <- glm(default ~ .-default,
               data = train,
               family = binomial)

#XGBoost
m_xgb <- 


#Random forest
m_rand <- 


# Predictions
preds <- tibble("logit_s" = predict(m_logit, xtest, type = "response"))

preds <- preds %>%
  mutate(logit_p = as.factor(if_else(logit_s > 0.5, 1, 0)))

confusionMatrix(preds$logit_p, ytest)

rocplot <- function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
  area <- auc(truth, pred)
  area <- format(round(area, 4), nsmall = 4)
  text(x=0.8, y=0.1, labels = paste("AUC =", area))
  segments(x0=0, y0=0, x1=1, y1=1, col="gray", lty=2)
}

rocplot(preds, test$default, col="blue")

