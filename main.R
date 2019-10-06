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
library(outliers)
library(mltools)
library(onehot)
library(BBmisc)
library(scales)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# Import data
# data <- read_delim("pd_data_v2.csv",
#                    ";", escape_double = FALSE,
#                    col_types = cols(default = col_factor(levels = c("0", "1"))),
#                    trim_ws = TRUE)

data <- read.csv("pd_data_v2.csv", sep = ";")
data$default <- as.factor(data$default)


# Preprocessing

# Outlier treatment
data_outliers <- data %>%
  filter_all(any_vars(. %in% c(-1e19, 1e19))) #filtering error values

data_filtered <- setdiff(data, data_outliers)

data %>%
  group_by(default) %>% tally()

data_filtered %>%
  group_by(default) %>% tally()

# Feature engineering

# Creating dummies
data_filtered <- data_filtered %>% #dummy variables, 1 if negative, 0 if positive
  mutate(d_pm_neg = if_else(profit_margin < 0, 1, 0)) %>%
  mutate(d_om_neg = if_else(operating_margin < 0, 1, 0)) %>%
  mutate(d_em_neg = if_else(EBITDA_margin < 0, 1, 0))







# Train test split
set.seed(123)
train.index <- createDataPartition(data$default, p = .75, list = FALSE) #stratified sampling
train <- data[ train.index,]
test  <- data[-train.index,]


quantiles <- tibble()
for (i in 1:ncol(train)) {
  
  
  
  
}


quantiles <- bind_rows(quantiles, c("low" = quantile(data_filtered[,2], probs = c(.05, .95))[[1]],
                                    "high" = quantile(data_filtered[,2], probs = c(.05, .95))[[2]]))
min(Winsorize(data_filtered$profit_margin, probs = c(.05, .95)))



