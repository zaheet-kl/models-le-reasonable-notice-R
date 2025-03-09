library(MASS)
library(caret)
library(mgcv)
library(sjPlot)
library(arm)
library(ordinal)
library(EnvStats)
library(rpartScore)
library(dplyr)
library(gbm)
library(MLmetrics)
library(e1071)

df <- read.csv('./Reasonable Notice/reasonable_notice.csv', stringsAsFactors = TRUE)
nrow(df)

df <- df[-which(df$classification == 'Remanded' | df$doNotML == 'true'),]
nrow(df)


source('./Reasonable Notice/training_scripts/anchor_prediction.R')



df$X1.1[which(df$X1.1 != 'Alberta' & df$X1.1 != 'British Columbia' & 
                df$X1.1 != 'Federal' & df$X1.1 != 'Quebec' &
                df$X1.1 != 'Ontario')] <- 'Yukon'

table(df$X1.1)

df[c('classification')] <- lapply(df[c('classification')], as.character)
df[c('classification')] <- lapply(df[c('classification')], as.numeric)

df[c('X1.2')] <- lapply(df[c('X1.2')], as.numeric)
df[c('X2.8')] <- lapply(df[c('X2.8')], as.numeric)
df[c('X3.2')] <- lapply(df[c('X3.2')], as.numeric)
df[c('X4.1')] <- lapply(df[c('X4.1')], as.numeric)
df[c('X4.2')] <- lapply(df[c('X4.2')], as.numeric)
df[c('X5.2')] <- lapply(df[c('X5.2')], as.numeric)
df[c('X5.3')] <- lapply(df[c('X5.3')], as.numeric)
df[c('X5.4')] <- lapply(df[c('X5.4')], as.numeric)
df[c('X6.1')] <- lapply(df[c('X6.1')], as.numeric)
df[c('X6.2')] <- lapply(df[c('X6.2')], as.numeric)
df[c('X6.3')] <- lapply(df[c('X6.3')], as.numeric)



df <- dplyr::select(df, 'id','year', 'classification', 'X1.1', 'X1.2', 'X1.3','X2.1.1_customChoiceValue',
             'X2.2', 'X2.3', 'X2.4', 'X2.5', 'X2.8',
             'X3.2', 'X4.1', 'X4.1.1', 'X4.2', 'X5.1', 'X5.2', 'X5.3', 'X5.4', 'X6.1', 'X6.2', 'X6.3')

df[c('X2.3log')] <- asinh(df$X2.3)
df[c('X5.1log')] <- asinh(df$X5.1)
df[c('ratio')] <- df$classification/df$X2.3
nrow(df)



var.mono_express <- c(0, 
                      0, 1, 1, 1,
                      1, 1, 
                      1)

var.mono_2425 <- c(0, 0, 0,
                   0, 1, 1, 1,
                   1,
                   1, 1, 1,
                   1, 1, 0, -1,
                   0, -1, -1)

var.mono_full <- c(0, 0, 0,
                   0, 1, 1, 1, 1, 1,
                   1,
                   1, 1, 1,
                   1, 1, 0, -1,
                   0, -1, -1)

set.seed(1)
folds <- createFolds(df$classification, k=5)

for (i in seq(1,length(folds))){
  
  # df_train <- df[-unlist(folds[i]),]
  # df_test <- df[unlist(folds[i]),]
  
  df_train <- df[unlist(folds[i]),]
  
  gbm_express <- gbm(classification ~ X1.1 + 
                       X2.1.1_customChoiceValue + X2.2 + X2.3 + X2.8 +
                       X4.1 + X4.1.1 + 
                       X5.1,
                     data=df_train, n.trees=300, shrinkage=0.1, n.minobsinnode=10,
                     interaction.depth=15, var.monotone = var.mono_express)
  
  gbm_2425 <- gbm(classification ~ X1.1 + X1.2 + X1.3 + 
                    X2.1.1_customChoiceValue + X2.2 + X2.3 + X2.8 +
                    X3.2 +
                    X4.1 + X4.1.1 + X4.2 + 
                    X5.1 + X5.2 + X5.3 + X5.4 +
                    X6.1 + X6.2 + X6.3,
                  data=df_train, n.trees=400, shrinkage=0.05, n.minobsinnode=5,
                  interaction.depth=11, var.monotone = var.mono_2425)
  
  gbm_full <- gbm(classification ~ X1.1 + X1.2 + X1.3 + 
                    X2.1.1_customChoiceValue + X2.2 + X2.3 + X2.4 + X2.5 + X2.8 +
                    X3.2 +
                    X4.1 + X4.1.1 + X4.2 + 
                    X5.1 + X5.2 + X5.3 + X5.4 +
                    X6.1 + X6.2 + X6.3,
                  data=df_train, n.trees=1500, shrinkage=0.01, n.minobsinnode=3,
                  interaction.depth=11, var.monotone = var.mono_full)
  
  ##### EXPRESS #####
  pred_train <- predict(gbm_express, n.trees=50)
  r2_express_train <- R2_Score(y_pred = pred_train, y_true = df_train$classification)
  rmse_express_train <- RMSE(y_pred = pred_train, y_true = df_train$classification)
  mae_express_train <- MAE(y_pred = pred_train, y_true = df_train$classification)
  
  pred_test <- predict(gbm_express, n.trees=50, newdata=df_test)
  r2_express_test <- R2_Score(y_pred = pred_test, y_true = df_test$classification)
  rmse_express_test <- RMSE(y_pred = pred_test, y_true = df_test$classification)
  mae_express_test <- MAE(y_pred = pred_test, y_true = df_test$classification)
  
  pred_express_full <- predict(gbm_express, n.trees=50, newdata=df)
  r2_express_full <- R2_Score(y_pred = pred_express_full, y_true = df$classification)
  rmse_express_full <- RMSE(y_pred = pred_express_full, y_true = df$classification)
  mae_express_full <- MAE(y_pred = pred_express_full, y_true = df$classification)
  
  anchor_express <- anchor_prediction(pred_express_full, df)
  
  ##### 2425 #####
  pred_train <- predict(gbm_2425, n.trees=150)
  r2_2425_train <- R2_Score(y_pred = pred_train, y_true = df_train$classification)
  rmse_2425_train <- RMSE(y_pred = pred_train, y_true = df_train$classification)
  mae_2425_train <- MAE(y_pred = pred_train, y_true = df_train$classification)
  
  pred_test <- predict(gbm_2425, n.trees=150, newdata=df_test)
  r2_2425_test <- R2_Score(y_pred = pred_test, y_true = df_test$classification)
  rmse_2425_test <- RMSE(y_pred = pred_test, y_true = df_test$classification)
  mae_2425_test <- MAE(y_pred = pred_test, y_true = df_test$classification)
  
  pred_2425_full <- predict(gbm_2425, n.trees=150, newdata=df)
  r2_2425_full <- R2_Score(y_pred = pred_2425_full, y_true = df$classification)
  rmse_2425_full <- RMSE(y_pred = pred_2425_full, y_true = df$classification)
  mae_2425_full <- MAE(y_pred = pred_2425_full, y_true = df$classification)
  
  anchor_2425 <- anchor_prediction(pred_2425_full, df)
  
  ##### FULL #####
  pred_train <- predict(gbm_full, n.trees=1500)
  r2_full_train <- R2_Score(y_pred = pred_train, y_true = df_train$classification)
  rmse_full_train <- RMSE(y_pred = pred_train, y_true = df_train$classification)
  mae_full_train <- MAE(y_pred = pred_train, y_true = df_train$classification)
  
  pred_test <- predict(gbm_full, n.trees=1500, newdata=df_test)
  r2_full_test <- R2_Score(y_pred = pred_test, y_true = df_test$classification)
  rmse_full_test <- RMSE(y_pred = pred_test, y_true = df_test$classification)
  mae_full_test <- MAE(y_pred = pred_test, y_true = df_test$classification)
  
  pred_full_full <- predict(gbm_full, n.trees=1500, newdata=df)
  r2_full_full <- R2_Score(y_pred = pred_full_full, y_true = df$classification)
  rmse_full_full <- RMSE(y_pred = pred_full_full, y_true = df$classification)
  mae_full_full <- MAE(y_pred = pred_full_full, y_true = df$classification)
  
  anchor_full <- anchor_prediction(pred_full_full, df)
  
  print(r2_express_full)
  print(r2_2425_full)
  print(r2_full_full)
  
  df_metric <- data.frame('R2_train'=c(r2_express_train, r2_2425_train, r2_full_train),
                          'R2_test'=c(r2_express_test, r2_2425_test, r2_full_test),
                          'R2_full'=c(r2_express_full, r2_2425_full, r2_full_full),
                          'RMSE_train'=c(rmse_express_train, rmse_2425_train, rmse_full_train),
                          'RMSE_test'=c(rmse_express_test, rmse_2425_test, rmse_full_test),
                          'RMSE_full'=c(rmse_express_full, rmse_2425_full, rmse_full_full),
                          'MAE_train'=c(mae_express_train, mae_2425_train, mae_full_train),
                          'MAE_test'=c(mae_express_test, mae_2425_test, mae_full_test),
                          'MAE_full'=c(mae_express_full, mae_2425_full, mae_full_full),
                          row.names=c('express', '2425', 'full'))
  
  # write.csv(df_metric, paste('optimized_fold_', i, '.csv', sep=''))
  
  
  group <- data.frame(id=double(nrow(df)))
  
  group$id[which(df$id %in% df_train$id)] <- 'train'
  group$id[which(df$id %in% df_test$id)] <- 'test'
  
  
  df_predictions <- data.frame('ids'=df$id,
                               'classification'=df$classification,
                               'new'=pred_full_full,
                               'new_anchor'=anchor_full,
                               '2425'=pred_2425_full,
                               '2425_anchor'=anchor_2425,
                               'express'=pred_express_full,
                               'express_anchor'=anchor_express,
                               'group'=group)
  
  colnames(df_predictions) <- c('ids','classification', 'new', 'new_anchor', '2425', '2425_anchor', 'express', 'express_anchor')
  
  # write.csv(df_predictions, paste('optimized_fold_', i, '.csv', sep=''),
  #           row.names=FALSE)
  
}



