prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

trade_or_business_model <- readRDS(prophetPath('models', 'usTaxForesight', 'tradeOrBusiness', 'tob_lr_v1.rds'))

TRADE_OR_BUSINESS <- 'Trade or business'
NOT_A_TRADE_OR_BUSINESS <- 'Not a trade or business'

trade_or_business_factors <- function(df){
  
  return(c(
    "1.1_customChoiceValue",
    "1.2_customChoiceValue",
    "1.3_customChoiceValue",
    "2.1",
    "2.2",
    "2.3",
    "2.4",
    "2.5",
    "2.6",
    "2.7",
    "3.1",
    "3.1.01",
    "3.2_customChoiceValue",
    "3.3",
    "3.4",
    "3.5",
    "4.1",
    "4.2_customChoiceValue",
    "4.1.01",
    "4.3_customChoiceValue",
    "4.4_customChoiceValue",
    "5.1",
    "5.2_customChoiceValue",
    "5.3",
    "5.4"   
  ))
}


trade_or_business_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, trade_or_business_factors, FALSE, answered_questions)

  df$X1.1_customChoiceValue <- as.factor(df$X1.1_customChoiceValue)
  df$X1.2_customChoiceValue <- as.factor(df$X1.2_customChoiceValue)
  df$X1.3_customChoiceValue <- as.factor(df$X1.3_customChoiceValue)
  df$X2.1 <- as.factor(df$X2.1)
  df$X2.2 <- as.factor(df$X2.2)
  df$X2.3 <- as.factor(df$X2.3)
  df$X2.4 <- as.factor(df$X2.4)
  df$X2.5 <- as.factor(df$X2.5)
  df$X2.6 <- as.factor(df$X2.6)
  df$X2.7 <- as.factor(df$X2.7)
  df$X3.1 <- as.factor(df$X3.1)
  df$X3.2_customChoiceValue <- as.factor(df$X3.2_customChoiceValue)
  df$X3.3 <- as.factor(df$X3.3)
  df$X3.5 <- as.factor(df$X3.5)
  df$X4.1 <- as.factor(df$X4.1)
  df$X4.2_customChoiceValue <- as.factor(df$X4.2_customChoiceValue)
  df$X4.3_customChoiceValue <- as.factor(df$X4.3_customChoiceValue)
  df$X4.4_customChoiceValue <- as.factor(df$X4.4_customChoiceValue)
  df$X5.2_customChoiceValue <- as.factor(df$X5.2_customChoiceValue)
  df$X5.4 <- as.factor(df$X5.4)
  
  df$X5.2_customChoiceValue <- ordered(df$X5.2_customChoiceValue,
                                       levels = c("Less often than once a week",
                                                  "Less than daily, about once a week",
                                                  "Daily"))
  
  df$X3.1.01 <- as.numeric(df$X3.1.01)
  df$X3.4 <- as.numeric(df$X3.4)
  df$X4.1.01 <- as.numeric(df$X4.1.01)
  df$X5.1 <- as.numeric(df$X5.1)
  df$X5.3 <- as.numeric(df$X5.3)
  
  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, trade_or_business_factors, TRUE, answered_questions)

  return(df)
}

trade_or_business_predict <- function(df) {
  
  df <- trade_or_business_transform_data(df)
  
  pred_prob <- predict(trade_or_business_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, TRADE_OR_BUSINESS, NOT_A_TRADE_OR_BUSINESS)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

trade_or_business_interim <- function(df){
  
  df <- trade_or_business_transform_data(df)
  
  return(create_interim_predictor(trade_or_business_model,
                                  NOT_A_TRADE_OR_BUSINESS,
                                  TRADE_OR_BUSINESS,
                                  data.frame(),
                                  0.75)(df))
}

