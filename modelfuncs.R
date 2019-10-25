# Data matrix conversions
toMatrix <- function(df){
  tf <- df
  tf$default <- as.numeric(as.character(tf$default))
  tf <- data.matrix(tf, rownames.force = NA)
  rownames(tf) <- NULL
  return(tf)}

ifExistsElseNull <- function(x){
  return(if(exists(deparse(substitute(x)))) {x} else {""})}


# Model performance evaluation and writing metrics to csv file
evaluateAndWrite <- function(prob, model.name, output.file) {
  
  pred <- if_else(prob >= threshold, 1, 0) #model predictions using threshold
  conm <- confusionMatrix(as.factor(pred), test$default, positive = "1") #confusion matrix
  mroc <- roc(test$default, prob) #roc object
  modeloutput <- tibble("nn" = conm$table[1],
                        "pn" = conm$table[2],
                        "np" = conm$table[3],
                        "pp" = conm$table[4],
                        "Sensitivity" = conm$byClass[[1]],
                        "Specificity" = conm$byClass[[2]],
                        "Accuracy" = conm$overall[[1]],
                        "Balanced_Accuracy" = conm$byClass[[11]],
                        "AUC" = auc(mroc),
                        "Threshold" = threshold,
                        "Resample" = resample,
                        "trees" = ifExistsElseNull(mtrees),
                        "mtry" = ifExistsElseNull(mmtry),
                        "nrounds" = ifExistsElseNull(nrounds),
                        "gamma" = ifExistsElseNull(mgamma),
                        "max_depth" = ifExistsElseNull(max_depth),
                        "eta" = ifExistsElseNull(eta),
                        "min_child_weight" = ifExistsElseNull(min_child_weight),
                        "subsample" = ifExistsElseNull(subsample),
                        "colsample_bytree" = ifExistsElseNull(colsample_bytree),
                        "Model" = model.name,
                        "Time_seconds" = round(Sys.time() - time.start, 2),
                        "Executed" = Sys.time())
  write.table(modeloutput,
              file = output.file,
              append = T,
              row.names = F,
              col.names = !file.exists(output.file),
              sep = ",")}


# Model performance evaluation and writing metrics to csv file
evaluateAndWriteLog <- function(prob, output.file) {
  
  pred <- if_else(prob >= threshold, 1, 0) #model predictions using threshold
  conm <- confusionMatrix(as.factor(pred), test$default, positive = "1") #confusion matrix
  mroc <- roc(test$default, prob) #roc object
  modeloutput <- tibble("nn" = conm$table[1],
                        "pn" = conm$table[2],
                        "np" = conm$table[3],
                        "pp" = conm$table[4],
                        "Sensitivity" = conm$byClass[[1]],
                        "Specificity" = conm$byClass[[2]],
                        "Accuracy" = conm$overall[[1]],
                        "Balanced_Accuracy" = conm$byClass[[11]],
                        "AUC" = auc(mroc),
                        "Threshold" = threshold,
                        "Resample" = resample,
                        "Model" = "log",
                        "Time_seconds" = Sys.time() - time.start,
                        "Executed" = Sys.time())
  write.table(modeloutput,
              file = output.file,
              append = T,
              row.names = F,
              col.names = !file.exists(output.file),
              sep = ",")}


# Model performance evaluation and writing metrics to csv file
evaluateAndWriteRan <- function(prob, output.file) {
  
  pred <- if_else(prob >= threshold, 1, 0) #model predictions using threshold
  conm <- confusionMatrix(as.factor(pred), test$default, positive = "1") #confusion matrix
  mroc <- roc(test$default, prob) #roc object
  modeloutput <- tibble("nn" = conm$table[1],
                        "pn" = conm$table[2],
                        "np" = conm$table[3],
                        "pp" = conm$table[4],
                        "Sensitivity" = conm$byClass[[1]],
                        "Specificity" = conm$byClass[[2]],
                        "Accuracy" = conm$overall[[1]],
                        "Balanced_Accuracy" = conm$byClass[[11]],
                        "AUC" = auc(mroc),
                        "Threshold" = threshold,
                        "Resample" = resample,
                        "Trees" = trees,
                        "Mtry" = mtry,
                        "Model" = "ran",
                        "Time_seconds" = Sys.time() - time.start,
                        "Executed" = Sys.time())
  write.table(modeloutput,
              file = output.file,
              append = T,
              row.names = F,
              col.names = !file.exists(output.file),
              sep = ",")}

# Model performance evaluation and writing metrics to csv file
evaluateAndWriteXgb <- function(prob, output.file) {
  
  pred <- if_else(prob >= threshold, 1, 0) #model predictions using threshold
  conm <- confusionMatrix(as.factor(pred), test$default, positive = "1") #confusion matrix
  mroc <- roc(test$default, prob) #roc object
  modeloutput <- tibble("nn" = conm$table[1],
                        "pn" = conm$table[2],
                        "np" = conm$table[3],
                        "pp" = conm$table[4],
                        "Sensitivity" = conm$byClass[[1]],
                        "Specificity" = conm$byClass[[2]],
                        "Accuracy" = conm$overall[[1]],
                        "Balanced_Accuracy" = conm$byClass[[11]],
                        "AUC" = auc(mroc),
                        "Threshold" = threshold,
                        "Resample" = resample,
                        "nrounds" = nrounds,
                        "gamma" = gamma,
                        "max_depth" = max_depth,
                        "eta" = eta,
                        "min_child_weight" = min_child_weight,
                        "subsample" = subsample,
                        "colsample_bytree" = colsample_bytree,
                        "Model" = "xgb",
                        "Time_seconds" = Sys.time() - time.start,
                        "Executed" = Sys.time())
  write.table(modeloutput,
              file = output.file,
              append = T,
              row.names = F,
              col.names = !file.exists(output.file),
              sep = ",")}


# Get training data
getTrainingData <- function(resample, model){
  
  if (model == "lg") {
    data_train <- read_csv("data_train_lg.csv")
  } else if (model == "xg") {
    data_train <- read_csv("data_train_xg.csv")
  } else if (model == "rf") {
    data_train <- read_csv("data_train_rf.csv")
  } else {stop("Unknown model param")}
  
  
  data_train$default <- as.factor(data_train$default)
  
  # Training data
  if (resample == "no") { #using raw unbalanced training dataset
    
    return(data_train)
    
  } else if (resample == "under") { #undersampling
    
    # Balancing training set (undersampling / equal size sampling)
    train_under <- downSample(data_train, data_train$default)
    train_under <- select(train_under, -c("Class")) #drop column
    
    return(train_under)
    
  } else if (resample == "over") { #oversampling
    
    # SMOTE oversampling
    perc.over <- (summary(data_train$default)[[1]] / summary(data_train$default)[[2]] - 1) * 100 #percentage needed to balance classes
    train_over <- SMOTE(default ~ ., as.data.frame(data_train), perc.over = perc.over, perc.under = 100)
    
    return(train_over)
    
  } else {stop("Unknown resample param")}
}



plot_ranger_importance <- function(model){
  importance(model) %>% 
    enframe() %>% 
    ggplot(aes(x = fct_reorder(name, value), y = value)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(x = NULL, title = "Variable importance for Ranger")}


