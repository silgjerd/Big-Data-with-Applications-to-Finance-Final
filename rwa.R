library(stats)
library(tidyverse)
library(patchwork)

graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# Import data
data <- read_csv("probs.csv")

datalong <- data #copy

# Spreading
data$obsnum <- c(1:(nrow(data)/nlevels(as.factor(data$model))),1:(nrow(data)/nlevels(as.factor(data$model))),1:(nrow(data)/nlevels(as.factor(data$model)))) #ugly hack to spread data
data <- data %>% pivot_wider(id_cols = obsnum,
                             names_from = model,
                             values_from = c(probs, actual))


# =========================================================================================================

# Function to calculate RWA
calculateRWA <- function(pd, lgd, ead, m, s) {
  
  # Correlation
  r <- 0.12 * (1 - exp(-50 * pd)) / (1 - exp(-50)) + 0.24 * ( 1 - (1 - exp(-50 * pd)) / (1 - exp(-50))  )
  
  # Maturity adjustment
  b <- (0.11852 - 0.05478 * log(pd)) ^ 2
  
  # Capital requirement
  k <- ( lgd * pnorm((1 - r)^-0.5 * qnorm(pd) + (r / (1 - r))^0.5 * qnorm(0.999))
         - pd * lgd) * (1 - 1.5 * b)^-1 * (1 + (m - 2.5) * b)
  
  # N(x) denotes the cumulative distribution function for a standard normal random variable 
  # (i.e. the probability that a normal random variable with mean zero and variance of one is less than or equal to x). 
  
  # G(z) denotes the inverse cumulative distribution function for a standard normal random variable (i.e. the value of x such that
  # N(x) = z). The normal cumulative distribution function and the inverse of the normal cumulative distribution
  # function are, for example, available in Excel as the functions NORMSDIST and NORMSINV.
  
  
  # RWA
  rwa <- k * 12.5 * ead
  
  # Imputing 0 to nan values
  rwa[is.nan(rwa)] <- 0
  
  return(rwa)
  
}


# Probability of default
logpd <- data$probs_log
xgbpd <- data$probs_xgb
ranpd <- data$probs_ran


# Params
lgd <- 0.45
ead <- 100
m <- 2.5
s <- 50

# Using function to calculate RWA using each of the three models' PDs
rwadf <- tibble("log" = calculateRWA(logpd, lgd, ead, m, s),
                "xgb" = calculateRWA(xgbpd, lgd, ead, m, s),
                "ran" = calculateRWA(ranpd, lgd, ead, m, s))

# Reporting mean value for each column (model)
round(colMeans(rwadf), 2)






# PLOTTING ----------


# Plot for comparing preds
plotdata <- data %>% select(c("probs_ran", "probs_xgb", "actual_ran"))
names(plotdata)[3] <- "Default"
plotdata$Default <- as.factor(plotdata$Default)

ggplot(plotdata, aes()) +
  geom_point(aes(x = probs_ran, y = probs_xgb, col = Default)) +
  theme_classic() +
  labs(x = "Random Forest", y = "XGBoost", title = "Probabilities") +
  scale_color_manual(values=c("#000000", "#c92a1e")) +
  theme(legend.position = c(0.9, 0.1))

plotdata$diff <- plotdata$probs_ran - plotdata$probs_xgb

plotdata <- plotdata %>%
  filter(abs(diff) > 2*sd(diff))




# Histogram plots
# p1
plot_multi_histogram <- function(df, feature, label_column) { #stolen with <3 from stackoverflow
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black", bins = 30) +
    geom_vline(aes(xintercept=mean(data$probs_log)), color="#eb4034", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=mean(data$probs_ran)), color="#40CD65", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=mean(data$probs_xgb)), color="#8AB8F5", linetype="dashed", size=1) +
    labs(x= "Probabilties", y = "Density", title = "Distribution of probabilities") +
    theme_classic() +
    theme(legend.position = c(0.87,0.8))
  plt + guides(fill=guide_legend(title=label_column))
}

names(datalong)[1] <- "Model"
p1 <- plot_multi_histogram(datalong, "probs", "Model")


# p2
rwalong <- rwadf %>% 
  gather(key = "Model", value = "Probability")

plot_multi_histogram <- function(df, feature, label_column) { #stolen with <3 from stackoverflow
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black", bins = 30) +
    geom_vline(aes(xintercept=mean(rwadf$log)), color="#eb4034", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=mean(rwadf$ran)), color="#40CD65", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=mean(rwadf$xgb)), color="#8AB8F5", linetype="dashed", size=1) +
    labs(x= "RWA", y = "", title = "Distribution of RWAs") +
    theme_classic() +
    theme(legend.position = c(0.1,0.8))
  plt + guides(fill=guide_legend(title=label_column))
}

p2 <- plot_multi_histogram(rwalong, "Probability", "Model")


# Using patchwork to combine plots
p1 + p2








