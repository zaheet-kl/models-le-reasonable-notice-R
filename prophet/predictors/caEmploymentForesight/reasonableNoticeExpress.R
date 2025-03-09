reasonable_notice_express_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'reasonableNoticeExpress', 'rn_express.rds'))

reasonable_notice_express_anchor_prediction <- function(pred_full, df){

  pred_anchor <- data.frame(pred=double(length(pred_full)))

  for (i in seq(1,length(pred_full))){

    if (df$X2.1.1_customChoiceValue[i]=='Upper management' | df$X2.8[i]==2 | df$X2.3[i] >= 10 |
        (df$X2.3[i]<4 & df$X4.1[i]==2 & df$X4.1.1[i] >=6)){

      if (df$X2.3[i] <= 1){
        pred_anchor$pred[i] <- pred_full[i] - 3
      } else if (df$X2.3[i] <= 2){
        pred_anchor$pred[i] <- pred_full[i] - 2
      } else if (df$X2.3[i] <= 5){
        pred_anchor$pred[i] <- pred_full[i] - 1
      } else {
        pred_anchor$pred[i] <- pred_full[i]
      }
    } else {
      pred_anchor$pred[i] <- ((10 - df$X2.3[i])/10)*(4.3*df$X2.3[i]) + (df$X2.3[i]/10)*pred_full[i]
    }
  }
  return(pred_anchor)
}

reasonable_notice_express_transform_data <- function(df){

  df <- format_dataframe_numerics(df, c('X2.2', 'X2.3', 'X2.4', 'X2.5', 'X4.1.1', 'X5.1'))

  levels(df$X1.1) <- c(levels(df$X1.1), 'Other')

  df$X1.1[which(df$X1.1 != 'Alberta' &
                df$X1.1 != 'British Columbia' &
                df$X1.1 != 'Federal' &
                df$X1.1 != 'Quebec' &
                df$X1.1 != 'Ontario')] <- 'Other'

  df$X1.2 <- ifelse(df$X1.2=='Yes', 2, 1)
  df$X2.8 <- ifelse(df$X2.8=='Yes', 2, 1)
  df$X3.2 <- ifelse(df$X3.2=='Yes', 2, 1)
  df$X4.1 <- ifelse(df$X4.1=='Yes', 2, 1)
  df$X4.2 <- ifelse(df$X4.2=='Yes', 2, 1)
  df$X5.2 <- ifelse(df$X5.2=='Yes', 2, 1)
  df$X5.3 <- ifelse(df$X5.3=='Yes', 2, 1)
  if ('X5.4' %in% colnames(df)){
    df$X5.4 <- ifelse(df$X5.4=='Yes', 2, 1)
  }
  df$X6.1 <- ifelse(df$X6.1=='Yes', 2, 1)
  df$X6.2 <- ifelse(df$X6.2=='Yes', 2, 1)
  df$X6.3 <- ifelse(df$X6.3=='Yes', 2, 1)

  df <- droplevels(df)

  return(df)
}

reasonable_notice_express_factors <- function(){
  labels <- reasonable_notice_express_model$var.names
  return(gsub('^X', '', labels))
}


reasonable_notice_express_predict <- function(df) {

  df <- reasonable_notice_express_transform_data(df)

  pred_full <- predict(reasonable_notice_express_model, n.trees=1500, newdata=df)
  pred_anchor <- reasonable_notice_express_anchor_prediction(pred_full, df)$pred

  return(list(
    type='number',
    predictedNumber=pred_anchor
  ))
}


