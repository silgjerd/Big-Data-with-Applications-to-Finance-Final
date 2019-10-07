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
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# Import data
data <- read.csv("pd_data_v2.csv", sep = ";")

data$default <- as.factor(data$default)
data$industry <- as.factor(data$industry)


# Preprocessing

# Bad data
data_outliers <- data %>%
  filter_all(any_vars(. %in% c(-1e19, 1e19))) #filtering error values
data_filtered <- setdiff(data, data_outliers) #removing values

data_filtered <- subset(data_filtered, total_assets >= 0) #removing impossible bad observations



data %>% group_by(default) %>% tally()

data_filtered %>% group_by(default) %>% tally()

# Feature engineering

# Creating dummies
data_filtered <- data_filtered %>% #dummy variables, 1 if negative, 0 if positive
  mutate(d_pm_neg = if_else(profit_margin    < 0, 1, 0)) %>%
  mutate(d_om_neg = if_else(operating_margin < 0, 1, 0)) %>%
  mutate(d_em_neg = if_else(EBITDA_margin    < 0, 1, 0))

# One hot encoding industry variable
industry_dmy <- dummyVars(" ~ industry", data = data_filtered)
industry_trsf <- data.frame(predict(industry_dmy, newdata = data_filtered)) #making new one hot encoded variables for each industry
data_filtered <- bind_cols(data_filtered, industry_trsf) #appending new variables
data_filtered <- select(data_filtered, -c("industry")) #drop original industry column











# Train test split
set.seed(123)
train.index <- createDataPartition(data_filtered$default, p = .75, list = FALSE) #stratified sampling
train <- data_filtered[ train.index,]
test  <- data_filtered[-train.index,]


# Winsorizing (capping)
wins_cols <- c(2,4:14,17,19,20,23) #columns to apply capping
for (i in wins_cols) {
  qs <- quantile(train[,i], probs = c(.05, .95)) #top and bottom quantiles
  train[,i] <- Winsorize(train[,i], minval = qs[1], maxval = qs[2]) #apply train data quantiles to both train and test data to avoid bias in cross validation
  test[,i]  <- Winsorize(test[,i],  minval = qs[1], maxval = qs[2])
}



# Balancing training set (downsampling / equal size sampling)
train <- downSample(train, train$default)
train <- select(train, -c("Class")) #drop column




# Write data
write.csv(train, file = "data_train.csv", row.names = F, append = F)
write.csv(test, file = "data_test.csv", row.names = F, append = F)





