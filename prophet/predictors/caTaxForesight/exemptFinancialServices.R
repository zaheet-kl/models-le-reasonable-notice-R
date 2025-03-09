prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

exempt_financial_services_model <- readRDS(prophetPath('models', 'caTaxForesight', 'exemptFinancialServices', 'efs_lr_v1.rds'))

NOT_ARRANGING_FS <- 'Not arranging for a financial service'
ARRANGING_FS <- 'Arranging for a financial service'

NONE_OF_THE_ABOVE_EFS <- 'None of the above'

NO_HARDCODE <- "__noHardcode__"

exempt_financial_services_factors <- function(df){
  
  labels <- attributes(exempt_financial_services_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('1.2')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}

exempt_financial_services_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, exempt_financial_services_factors, FALSE, answered_questions)
  
  if (df$X1.2 != NONE_OF_THE_ABOVE_EFS){
    pred_prob <- 1
    pred_label <- NOT_ARRANGING_FS
  } else {
    pred_prob <- 1
    pred_label <- NO_HARDCODE
  }
 
  return(list(
    prediction=pred_label,
    probability=pred_prob
  )) 
}

exempt_financial_services_predict <- function(df){
  
  hardcodes <- exempt_financial_services_logic(df)
  
  if (hardcodes$prediction != NO_HARDCODE){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(exempt_financial_services_model, newdata=df, type='response')
    pred_label <- ifelse(pred_prob>0.5, NOT_ARRANGING_FS, ARRANGING_FS)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

exempt_financial_services_interim <- function(df){
  
  hardcodes <- exempt_financial_services_logic(df)
  
  if (hardcodes$prediction != NO_HARDCODE){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(exempt_financial_services_model,
                                    ARRANGING_FS,
                                    NOT_ARRANGING_FS,
                                    data.frame(),
                                    1)(df))
  }
}


