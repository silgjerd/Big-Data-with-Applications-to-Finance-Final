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
resample <- "under" #"no" for original dataset, "under" for undersampling, "over", for oversampling

output.file <- "model_output.csv"


# Import data
train <- getTrainingData(resample)
dmtrain <- toMatrix(train)

test <- read_csv("data_test.csv")
test$default <- as.factor(test$default)


# Data formats (for xgb)
xtrain <- dmtrain[,-1]
ytrain <- dmtrain[,1]

xtest <- data.matrix(test[,-1])
ytest <- test[,1]

# ===================================================================







# Modelling =========================================================
trc <- trainControl(method = "repeatedcv",
                    number = 25,
                    repeats = 5,
                    verboseIter = TRUE,
                    returnData = FALSE,
                    returnResamp = "final",
                    savePredictions = FALSE,
                    classProbs = FALSE,
                    summaryFunction = defaultSummary,
                    selectionFunction = "best",
                    allowParallel = TRUE)

# Logistic regression
time.start <- Sys.time()
mlog <- train(default ~ .,
               data = train,
               trControl = trc,
               method = "glm",
               family = "binomial")

prob <- 
evaluateAndWrite(prob, "log")

# XGBoost
time.start <- Sys.time()

mxgb <-  train(default ~ .,
               data = train,
               method = "xgbTree",
               trControl = trc)




mxgb <- xgboost(data = xtrain,
                label = ytrain,
                nround = 100,
                objective = "binary:logistic",
                trControl = trc)

prob <- 
evaluateAndWrite(prob, "xgb")


# Random forest
time.start <- Sys.time()
mran <- train(default ~ .,
              data = train,
              method = "ranger",
              trControl = trc)

prob <- 
evaluateAndWrite(prob, "ran")


#====================================================================




ran_prob <- predict(mran, test)
ran_pred <- if_else(ran_prob >= threshold, 1, 0)
confm <- confusionMatrix(ran_prob, test$default, positive = "1")
confm$byClass[2]



# Predictions, model inference
preds <- tibble("log_prob" = predict(mlog, test, type = "prob")[,2],
                "xgb_prob" = predict(mxgb, xtest),
                "ran_prob" = predict(mran, test, type = "prob")[,2])

preds <- preds %>%
  mutate(log_pred = as.factor(if_else(log_prob > threshold, 1, 0))) %>%
  mutate(xgb_pred = as.factor(if_else(xgb_prob > threshold, 1, 0))) %>%
  mutate(ran_pred = as.factor(if_else(ran_prob > threshold, 1, 0)))


# Confusion matrix
confusionMatrix(preds$log_pred, test$default, positive = "1")
confusionMatrix(preds$xgb_pred, test$default, positive = "1")
confusionMatrix(preds$ran_pred, test$default, positive = "1")






# ROC plot
log_prob <- predict(mran, test, type = "prob")[,2]
xgb_prob <- predict(mxgb, xtest)
ran_prob <- predict(mran, test, type = "prob")[,2]


par(pty = "s")
roc(test$default, log_prob,
    plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Rate", ylab="True Positive Rate",
    col="#02818a", lwd=3, print.auc=TRUE, print.auc.x=45, print.auc.col="#67a9cf")
plot.roc(test$default, xgb_prob, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
plot.roc(test$default, ran_prob, percent=TRUE, col="#ed6842", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=32)



par(pty = "s")
roc(test$default, xgb_prob,
    plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Rate", ylab="True Positive Rate",
    col="#02818a", lwd=3, print.auc=TRUE, print.auc.x=45, print.auc.col="#67a9cf")

par(pty = "s")
roc(test$default, ran_prob,
    plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Rate", ylab="True Positive Rate",
    col="#02818a", lwd=3, print.auc=TRUE, print.auc.x=45, print.auc.col="#67a9cf")





legend("bottomright", legend=c("Logisitic Regression", "XGBoost", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)






# Train logistic regression model
mlog <- train(default ~ liq_dif_23,
              data = train_under,
              method = "glm",
              family = "binomial"
)

# Extract predicted probabilities
log_prob <- predict(mlog, test, type = "prob")[,2] 

# Plot ROC curve
par(pty = "s")
roc(test$default, log_prob, 
    plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Rate", ylab="True Positive Rate",
    col="#02818a", lwd=3, print.auc=TRUE, print.auc.x=45, print.auc.col="#67a9cf")


ggplot(train, aes()) +
  geom_histogram(aes(age_of_company, col = "black"), binwidth = 1, col = "black", fill = "dodgerblue") +
  theme_classic() +
  labs(x = "", y = "")







png("rplot.png", width = 500, height = 500)

par(pty = "s")
roc(test$default, log_prob,
    plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Rate", ylab="True Positive Rate",
    col="#02818a", lwd=3, print.auc=TRUE, print.auc.x=45, print.auc.col="#67a9cf")

dev.off()



# Write PDs for RWA calculation
probs <- preds[,1:3]
write.csv(probs, file = "probs.csv", row.names = F)



