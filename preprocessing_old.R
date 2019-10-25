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
library(tidymodels)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# Import data
data_raw <- read.csv("pd_data_v2.csv", sep = ";")

# Data types
data_raw$default <- as.factor(data_raw$default)
data_raw$industry <- as.factor(data_raw$industry)

data_raw[data_raw == 1e19] <- NA
data_raw[data_raw == -1e19] <- NA


# Bad data
data_outliers <- data_raw %>%
  filter_all(any_vars(. %in% c(-1e19, 1e19))) #filtering error values
data <- setdiff(data_raw, data_outliers) #removing values


# Feature engineering
data <- data %>%
  mutate(d_age_young = if_else(age_of_company <= 11, 1, 0)) %>% #company age dummy
  mutate(liq_dif_12 = liquidity_ratio_1 - liquidity_ratio_2) %>% #diff in liquidity ratios
  mutate(liq_dif_13 = liquidity_ratio_1 - liquidity_ratio_3) %>%
  mutate(liq_dif_23 = liquidity_ratio_2 - liquidity_ratio_3) %>%
  mutate(revenue = log(revenue)) %>% #log transformations
  mutate(amount_unpaid_debt = log(amount_unpaid_debt)) %>%
  mutate(unpaid_debt_collection = log(unpaid_debt_collection)) %>%
  mutate(paid_debt_collection = log(paid_debt_collection))
  


# One hot encoding industry variable
industry_dmy <- dummyVars(" ~ industry", data = data) #making dummy vars
industry_trsf <- data.frame(predict(industry_dmy, newdata = data)) #making new one hot encoded variables for each industry
data <- bind_cols(data, industry_trsf) #appending new variables
data <- select(data, -c("industry")) #drop original industry column





# Train test split
set.seed(123)
train.index <- createDataPartition(data$default, p = .75, list = FALSE) #stratified sampling
train <- data[ train.index,]
test  <- data[-train.index,]


# Write data
write.csv(train, file = "data_train_xr.csv", row.names = F)
write.csv(test, file = "data_test.csv", row.names = F)


# Winsorizing (capping)
wins_cols <- c(2, #pm
               4:11,
               15,
               16,
               19,
               20,
               22) #columns to apply capping
for (i in wins_cols) {
  qs <- quantile(train[,i], probs = c(.05, .95)) #top and bottom quantiles
  train[,i] <- Winsorize(train[,i], minval = qs[1], maxval = qs[2]) #apply train data quantiles to both train and test data to avoid data leakage
  test[,i]  <- Winsorize(test[,i],  minval = qs[1], maxval = qs[2])
}

# Write data
write.csv(train, file = "data_train_log.csv", row.names = F)
write.csv(test, file = "data_test.csv", row.names = F)



# # Hist plot
# train %>%
#   keep(is.numeric) %>% 
#   gather() %>% 
#   ggplot(aes(value)) +
#   facet_wrap(~ key, scales = "free") +
#   geom_histogram() +
#   theme_classic()









