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
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# Import data
data <- read_delim("pd_data_v2.csv",
                   ";", escape_double = FALSE,
                   col_types = cols(default = col_factor(levels = c("0", "1"))),
                   trim_ws = TRUE)



data %>%
  group_by(default) %>% tally()


# Outlier treatment
data_outliers <- data %>%
  filter_all(any_vars(. %in% c(-1e19, 1e19))) #filtering outliers

data_filtered <- setdiff(data, data_outliers)

data_filtered %>%
  group_by(default) %>% tally()


normalize1 <- function(x){(x-min(x))/(max(x)-min(x))}

for (i in 2:24) {data_normalized[,i] <- normalize1(data_filtered[,i])}

data_normalized <- data_filtered
data_normalized[,2] <- normalize1(data_filtered[,2])



# Train test split
set.seed(123)
train.index <- createDataPartition(data$default, p = .7, list = FALSE) #stratified sampling
train <- data[ train.index,]
test  <- data[-train.index,]







