prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

nonresident_trade_or_business_model <- readRDS(prophetPath('models', 'usTaxForesight', 'nonresidentTradeOrBusiness', 'nrtb_lr_v1.rds'))

NOT_ENGAGED <- 'Not engaged in trade or business in U.S.'
ENGAGED <- 'Engaged in trade or business in U.S.'

YES <- 'Yes'
NO <- 'No'

NONRESIDENT_ALIEN_INDIVIDUAL <- 'nonresident alien individual'
FOREIGN_CORPORATION <- 'foreign corporation'

nonresident_trade_or_business_factors <- function(){
  
  labels <- attributes(nonresident_trade_or_business_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('1.2', '1.2.1', '2.1', '2.1.1', '2.1.2', '2.1.3', '2.1.4',
                        '2.2', '2.2.1', '2.3', '2.3.1', '3.1')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}

nonresident_trade_or_business_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, nonresident_trade_or_business_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X2.1.3', 'X2.1.4'))
  
  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, nonresident_trade_or_business_factors, TRUE, answered_questions)
  
  return(df)
}

nonresident_trade_or_business_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, nonresident_trade_or_business_factors, FALSE, answered_questions)
  
  if (df$X3.1 == YES){
    pred_label <- NOT_ENGAGED
    pred_prob <- 1
  } else if (df$X1.2 == NONRESIDENT_ALIEN_INDIVIDUAL &
             df$X1.2.1 == YES){
    pred_label <- ENGAGED
    pred_prob <- 1
  } else if (df$X1.2 == NONRESIDENT_ALIEN_INDIVIDUAL &
             df$X2.1 == YES &
             (df$X2.1.1== YES | df$X2.1.2 == YES) &
             df$X2.1.3 <= 90 &
             df$X2.1.4 <= 3000){
    pred_label <- NOT_ENGAGED
    pred_prob <- 1
  } else if (df$X1.2 == NONRESIDENT_ALIEN_INDIVIDUAL &
             df$X2.1 == YES &
             ((df$X2.1.1== NO & df$X2.1.2 == NO) |
              ((df$X2.1.1== YES | df$X2.1.2 == YES) &
               df$X2.1.3 > 90 &
               df$X2.1.4 > 3000))){
    pred_label <- ENGAGED
    pred_prob <- 1
  } else if (df$X1.2 == FOREIGN_CORPORATION &
             df$X2.1 == YES){
    pred_label <- ENGAGED
    pred_prob <- 1
  } else if (df$X2.2 == YES &
             df$X2.2.1 == NO){
    pred_label <- NOT_ENGAGED
    pred_prob <- 1
  } else if (df$X2.3 == YES &
             df$X2.3.1 == NO){
    pred_label <- NOT_ENGAGED
    pred_prob <- 1
  } else {
    pred_label <- '__noHardcode__'
    pred_prob <- NA
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

nonresident_trade_or_business_predict <- function(df){
  
  df <- nonresident_trade_or_business_transform_data(df)
  
  hardcodes <- nonresident_trade_or_business_logic(df)
  
  if (hardcodes$prediction == ENGAGED |
      hardcodes$prediction == NOT_ENGAGED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(nonresident_trade_or_business_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, NOT_ENGAGED, ENGAGED)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

nonresident_trade_or_business_interim <- function(df){
  
  df <- nonresident_trade_or_business_transform_data(df)
  
  hardcodes <- nonresident_trade_or_business_logic(df)
  
  if (hardcodes$prediction == ENGAGED |
      hardcodes$prediction == NOT_ENGAGED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(nonresident_trade_or_business_model,
                                    ENGAGED,
                                    NOT_ENGAGED,
                                    data.frame(),
                                    1.5)(df))
  }
  
}


