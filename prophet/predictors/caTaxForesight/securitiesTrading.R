prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

securities_trading_model <- readRDS(prophetPath('models', 'caTaxForesight', 'securitiesTrading', 'sec_lr_v1.rds'))

securities_trading_constants <- data.frame(ON_ACCOUNT_OF_CAPITAL='On account of capital',
                                           INCOME_FROM_BUSINSS='Income from business',
                                           YES='Yes',
                                           NO='No',
                                           NO_HARDCODE = "__noHardcode__",
                                           stringsAsFactors = FALSE)

securities_trading_checkbox_questions <- c('X2.4.1.1', 'X2.4.1.2', 'X2.4.1.3',
                                           'X2.4.1.4', 'X2.4.1.5', 'X2.4.1.6')

securities_trading_factors <- function(){
  
  labels <- attributes(securities_trading_model$terms)$term.labels
  labels <- labels[!(labels %in% c('X2.4.1_any', 'X2.7log', 'X3.5log'))]
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('3.6', '3.6.1')
  
  labels <- c(labels, hardcode_factors, c('2.7', '3.5'), gsub('^X', '', securities_trading_checkbox_questions))
  
  return(labels)
}

securities_trading_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, securities_trading_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X1.4', 'X2.7', 'X3.5'))
  
  df$X2.4.1_any <- any(TRUE %in% df[securities_trading_checkbox_questions])
  
  if ('X2.7' %in% answered_questions){
    df$X2.7log <- log(df$X2.7 + 1)
  }
  
  if ('X3.5' %in% answered_questions){
    df$X3.5log <- log(df$X3.5 + 1)
  }
  
  df <- empty_question_transformation(df, securities_trading_factors, TRUE, answered_questions)
  
  return(df)
}

securities_trading_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, securities_trading_factors, FALSE, answered_questions)
  
  if (df$X3.6 == securities_trading_constants$YES & df$X3.6.1 == securities_trading_constants$YES){
    pred_label <- securities_trading_constants$ON_ACCOUNT_OF_CAPITAL
    pred_prob <- 1
  } else if (df$X3.6 == securities_trading_constants$YES & df$X3.6.1 == securities_trading_constants$NO){
    pred_label <- securities_trading_constants$INCOME_FROM_BUSINSS
    pred_prob <- 1
  } else {
    pred_label <- securities_trading_constants$NO_HARDCODE
    pred_prob <- NA
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

securities_trading_predict <- function(df){
  
  df <- securities_trading_transform_data(df)
  
  hardcodes <- securities_trading_logic(df)
  
    if (hardcodes$prediction != NO_HARDCODE){
      pred_prob <- hardcodes$probability
      pred_label <- hardcodes$prediction
    } else {
      pred_prob <- predict(securities_trading_model, newdata=df, type='response')
      pred_label <- ifelse(pred_prob>0.5,
                           securities_trading_constants$ON_ACCOUNT_OF_CAPITAL,
                           securities_trading_constants$INCOME_FROM_BUSINSS)
    }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

securities_trading_interim <- function(df){
  
  df <- securities_trading_transform_data(df)
  
  hardcodes <- securities_trading_logic(df)
  
  if (hardcodes$prediction != NO_HARDCODE){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(securities_trading_model,
                                    securities_trading_constants$INCOME_FROM_BUSINSS,
                                    securities_trading_constants$ON_ACCOUNT_OF_CAPITAL,
                                    data.frame(),
                                    2)(df))
  }
  
}

