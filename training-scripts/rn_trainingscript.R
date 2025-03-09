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

df <- read.csv('./reasonable_notice.csv', stringsAsFactors = TRUE)
nrow(df)

df <- df[-which(df$classification == 'Remanded' | df$doNotML == 'true'),]
nrow(df)

source('./anchor_prediction.R')

levels(df$X1.1) <- c(levels(df$X1.1), 'Other')

df$X1.1[which(df$X1.1 != 'Alberta' & df$X1.1 != 'British Columbia' & 
           df$X1.1 != 'Federal' & df$X1.1 != 'Quebec' &
           df$X1.1 != 'Ontario')] <- 'Other'

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

df <- droplevels(df)

df_train <- df[which(df$year <= 2016),]
df_test <- df[which(df$year > 2016),]



# ##################################
# ################################## Express

var.mono <- c(0,
              0, 1, 1, 1,
              1, 1,
              1)

set.seed(1)
gbm1 <- gbm(classification ~ X1.1 + 
              X2.1.1_customChoiceValue + X2.2 + X2.3 + X2.8 +
              X4.1 + X4.1.1 + 
              X5.1,
            data=df_train, n.trees=1300, shrinkage=0.005, n.minobsinnode=3,
            interaction.depth=11, var.monotone = var.mono)


pred_train <- predict(gbm1, n.trees=1300)
R2_Score(y_pred = pred_train, y_true = df_train$classification)
RMSE(y_pred = pred_train, y_true = df_train$classification)
MAE(y_pred = pred_train, y_true = df_train$classification)


pred_test <- predict(gbm1, n.trees=1300, newdata=df_test)
R2_Score(y_pred = pred_test, y_true = df_test$classification)
RMSE(y_pred = pred_test, y_true = df_test$classification)
MAE(y_pred = pred_test, y_true = df_test$classification)


pred_full <- predict(gbm1, n.trees=1300, newdata=df)
R2_Score(y_pred = pred_full, y_true = df$classification)
RMSE(y_pred = pred_full, y_true = df$classification)
MAE(y_pred = pred_full, y_true = df$classification)


###################################
################################### Full Model w/o 2.4, 2.5
var.mono <- c(0, 0, 0,
              0, 1, 1, 1,
              1,
              1, 1, 1,
              1, 1, 0, -1,
              0, -1, -1)

set.seed(5)
gbm1 <- gbm(classification ~ X1.1 + X1.2 + X1.3 + 
              X2.1.1_customChoiceValue + X2.2 + X2.3 + X2.8 +
              X3.2 +
              X4.1 + X4.1.1 + X4.2 + 
              X5.1 + X5.2 + X5.3 + X5.4 +
              X6.1 + X6.2 + X6.3,
            data=df_train, n.trees=400, shrinkage=0.05, n.minobsinnode=5,
            interaction.depth=11, var.monotone = var.mono)

n_tree <- 150

pred_train <- predict(gbm1, n.trees=n_tree)
R2_Score(y_pred = pred_train, y_true = df_train$classification)
RMSE(y_pred = pred_train, y_true = df_train$classification)
MAE(y_pred = pred_train, y_true = df_train$classification)


pred_test <- predict(gbm1, n.trees=n_tree, newdata=df_test)
R2_Score(y_pred = pred_test, y_true = df_test$classification)
RMSE(y_pred = pred_test, y_true = df_test$classification)
MAE(y_pred = pred_test, y_true = df_test$classification)

pred_full <- predict(gbm1, n.trees=n_tree, newdata=df)
R2_Score(y_pred = pred_full, y_true = df$classification)
RMSE(y_pred = pred_full, y_true = df$classification)
MAE(y_pred = pred_full, y_true = df$classification)



###################################
################################### Full Model
var.mono <- c(0, 0, 0,
              0, 1, 1, 1, 1, 1,
              1,
              1, 1, 1,
              1, 1, 0, -1,
              0, -1, -1)

set.seed(12)
gbm1 <- gbm(classification ~ X1.1 + X1.2 + X1.3 + 
              X2.1.1_customChoiceValue + X2.2 + X2.3 + X2.4 + X2.5 + X2.8 +
              X3.2 +
              X4.1 + X4.1.1 + X4.2 + 
              X5.1 + X5.2 + X5.3 + X5.4 +
              X6.1 + X6.2 + X6.3,
            data=df_train, n.trees=1500, shrinkage=0.005, n.minobsinnode=3,
            interaction.depth=11, var.monotone = var.mono)

n_tree <- 1500

check <- data.frame('X1.1'='Ontario', 'X1.2'=1, 'X1.3'='Employee','X2.1.1_customChoiceValue'='Labourer',
           'X2.2'=61, 'X2.3'=7, 'X2.4'=8, 'X2.5'=0, 'X2.8'=2,
           'X3.2'=1, 'X4.1'=1, 'X4.1.1'=1, 'X4.2'=1, 'X5.1'=45614, 'X5.2'=1, 'X5.3'=1,
           'X5.4'=1, 'X6.1'=1, 'X6.2'=1, 'X6.3'=1)

pred_check <- predict(gbm1, n.trees=n_tree, newdata = check)
pred_check
anchor_prediction(pred_check, check)


pred_train <- predict(gbm1, n.trees=n_tree)
R2_Score(y_pred = pred_train, y_true = df_train$classification)
RMSE(y_pred = pred_train, y_true = df_train$classification)
MAE(y_pred = pred_train, y_true = df_train$classification)

pred_test <- predict(gbm1, n.trees=n_tree, newdata=df_test)
R2_Score(y_pred = pred_test, y_true = df_test$classification)
RMSE(y_pred = pred_test, y_true = df_test$classification)
MAE(y_pred = pred_test, y_true = df_test$classification)

# Anchor test
pred_test_anchor <- anchor_prediction(pred_test, df_test)$pred 
R2_Score(y_pred = pred_test_anchor, y_true = df_test$classification)
RMSE(y_pred = pred_test_anchor, y_true = df_test$classification)
MAE(y_pred = pred_test_anchor, y_true = df_test$classification)

pred_full <- predict(gbm1, n.trees=n_tree, newdata=df)
R2_Score(y_pred = pred_full, y_true = df$classification)
RMSE(y_pred = pred_full, y_true = df$classification)
MAE(y_pred = pred_full, y_true = df$classification)



mean(abs(pred_test - df_test$classification)[which(abs(pred_test - df_test$classification) <30)])

#saveRDS(gbm1, '../prophet/models/caEmploymentForesight/ReasonableNotice/rn_best.rds')
# Save the model to the specified output path
# output_dir <- "./outputs/model"
# dir.create(output_dir, recursive = TRUE, showWarnings = FALSE) # create the output directory
# saveRDS(gbm1, file.path(output_dir, "rn_best.rds"))

args <- commandArgs(trailingOnly=TRUE)
output_dir <- args[1]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(gbm1, file.path(output_dir, "rn_best.rds"))

predict(gbm1, n.trees=1500, newdata=df[1,])

# Anchor full
pred_full_anchor <- anchor_prediction(pred_full, df)$pred
R2_Score(y_pred = pred_full_anchor, y_true = df$classification)
RMSE(y_pred = pred_full_anchor, y_true = df$classification)
MAE(y_pred = pred_full_anchor, y_true = df$classification)

##### Full prediction
pred_full <- predict(gbm1, n.trees=50, newdata=df)
full_df <- data.frame(pred_full, df$id)


#


