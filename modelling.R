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
train$default <- as.factor(train$default)
test$default <- as.factor(test$default)

m_log <- glm(default ~ .-default,
             data = train,
             family = binomial)

#XGBoost
m_xgb <- xgboost(data = data.matrix(train[,-1]),
                 label = data.matrix(train$default),
                 max.depth = 2,
                 eta = 1,
                 nthread = 2,
                 nround = 2,
                 objective = "binary:logistic")


#Random forest
m_ran <- randomForest(default ~ .-default,
                      data = train)


# Predictions, model inference
preds <- tibble("log_prob" = as.numeric(as.character(predict(m_log, test, type = "response"))),
                "xgb_prob" = predict(m_xgb, data.matrix(test[,-1])),
                "ran_prob" = predict(m_ran, test, type = "prob")[,2])




predict(m_ran, test, type = "prob")

preds <- preds %>%
  mutate(log_pred = as.factor(if_else(log_prob > 0.5, 1, 0))) %>%
  mutate(xgb_pred = as.factor(if_else(xgb_prob > 0.5, 1, 0))) %>%
  mutate(ran_pred = as.factor(if_else(ran_prob > 0.5, 1, 0)))


# Confusion matrix
confusionMatrix(preds$log_pred, test$default, positive = "1")
confusionMatrix(preds$xgb_pred, test$default, positive = "1")
confusionMatrix(preds$ran_pred, test$default, positive = "1")








# ROC plot
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


# Scatterplot
ggplot(preds, aes()) +
  geom_point(aes(x = log_prob, y = xgb_prob)) +
  theme_classic()





