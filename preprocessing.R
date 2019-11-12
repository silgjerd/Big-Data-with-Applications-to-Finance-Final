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
library(corrplot)
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
data <- read.csv("pd_data_v2.csv", sep = ";")

# Data types
data$default <- as.factor(data$default)
data$industry <- as.factor(data$industry)

# Bad data
data[data == 1e19] <- NA
data[data == -1e19] <- NA

data$unpaid_debt_collection[data$unpaid_debt_collection < 0] <- NA
data$paid_debt_collection[data$paid_debt_collection < 0] <- NA
data$amount_unpaid_debt[data$amount_unpaid_debt < 0] <- NA

# Train test split
set.seed(9)
train.index <- createDataPartition(data$default, p = .75, list = FALSE) #stratified sampling
train <- data[ train.index,]
test  <- data[-train.index,]


# Create recipe
rec <- recipe(default ~ ., data = train) %>%
  step_meanimpute(all_numeric()) %>%
  step_modeimpute(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_mutate(
    d_age_young = if_else(age_of_company <= 11, 1, 0),
    liq_dif_12 = liquidity_ratio_1 - liquidity_ratio_2,
    liq_dif_13 = liquidity_ratio_1 - liquidity_ratio_3,
    liq_dif_23 = liquidity_ratio_2 - liquidity_ratio_3,
    pmtr_over_pm = payment_reminders / profit_margin,
    aao_times_pmtr = adverse_audit_opinion * payment_reminders,
    udc_over_icr = unpaid_debt_collection / interest_coverage_ratio) %>%
  step_log(
    unpaid_debt_collection,
    paid_debt_collection,
    amount_unpaid_debt,
    offset = 1) %>%
  step_dummy(industry)

# Train the recipe on the training set
prec <- prep(rec, training = train)

# Bake the data (apply the recipe to get the final datasets)
mtrain <- juice(prec)
mtest <- bake(prec, new_data = test)

train <- as.data.frame(mtrain)
test <- as.data.frame(mtest)


# Write data (XGBoost)
write.csv(train, file = "data_train_xg.csv", row.names = F)
write.csv(test, file = "data_test_xg.csv", row.names = F)

# Write data (Random forest)
write.csv(train, file = "data_train_rf.csv", row.names = F)
write.csv(test, file = "data_test_rf.csv", row.names = F)



# Processing for logistic regression ===============

# Winsorizing (capping), logit is bad at dealing with outliers and missing data
wins_cols <- c("profit_margin", "operating_margin", "EBITDA_margin", "interest_coverage_ratio", "cost_of_debt",
               "interest_bearing_debt", "revenue_stability", "equity_ratio", "equity_ratio_stability", "equity",
               "total_assets", "unpaid_debt_collection", "paid_debt_collection", "amount_unpaid_debt",
               "pmtr_over_pm", "aao_times_pmtr", "udc_over_icr")
for (column in wins_cols) {
  qs <- quantile(train[,column], probs = c(.05, .95)) #top and bottom quantiles
  train[,column] <- Winsorize(train[,column], minval = qs[1], maxval = qs[2]) #apply train data quantiles to both train and test data to avoid data leakage
  test[,column]  <- Winsorize(test[,column],  minval = qs[1], maxval = qs[2])
}


# Dropping variables based on VIF
drop_cols <- c("liq_dif_12", "liq_dif_13", "liq_dif_23",
               "liquidity_ratio_1", "liquidity_ratio_2",
               "EBITDA_margin", "equity_ratio_stability",
               "total_assets", "d_age_young")

train <- train %>% select(-drop_cols)
test <- test %>% select(-drop_cols)

viftrain <- train
viftrain$default <- as.numeric(as.character(viftrain$default))

lmfit <- (lm(default ~ ., data = viftrain))
vif(lmfit)



# Write data (Logistic regression)
write.csv(train, file = "data_train_lg.csv", row.names = F)
write.csv(test, file = "data_test_lg.csv", row.names = F)






# PLOTTING ------------------------

# Hist plot
test %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() +
  theme_classic()


# Corr matrix
test$default <- as.numeric(as.character(test$default))
test <- test %>% select(1:23)


#1
mydata.cor <- cor(test)
corrplot(mydata.cor)

#2
palette = colorRampPalette(c("blue", "white", "red")) (20)
heatmap(x = mydata.cor, col = palette, symm = TRUE)






