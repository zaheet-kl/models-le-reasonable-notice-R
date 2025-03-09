prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

unrelated_business_income_model <- readRDS(prophetPath('models', 'usTaxForesight', 'unrelatedBusinessIncome', 'ubit_lr_v1.rds'))

UNRELATED_BUSINESS_INCOME <- 'Unrelated Business Income'
NOT_UNRELATED_BUSINESS_INCOME <- 'Not Unrelated Business Income'
EXCLUDED <- 'Excluded'

YES <- 'Yes'
NO <- 'No'

ubit_checkbox_questions <- c('5.3.1','5.3.2','5.3.3','5.3.4','5.3.5','5.3.6','5.3.7','5.3.8','5.3.9','5.3.10',
                             '5.3.11','5.3.12','5.3.13','5.3.14','5.3.15','5.3.16')

unrelated_business_income_factors <- function(){
  
  labels <- attributes(unrelated_business_income_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('2.1.1', '5.1', '5.2',
                        '2.1', '2.2', '3.2', '3.3')
  
  labels <- c(labels, hardcode_factors, ubit_checkbox_questions)
  
  return(labels)
}

unrelated_business_income_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, unrelated_business_income_factors, FALSE, answered_questions)
  
  if (df$X2.1.1 == YES){
    pred_label <- UNRELATED_BUSINESS_INCOME
    pred_prob <- 1
  } else if (df$X5.1 == YES | df$X5.2 == YES | any(df[,paste('X', ubit_checkbox_questions, sep='')] == TRUE)){
    pred_label <- EXCLUDED
    pred_prob <- 1
  } else if (df$X2.1 == NO | df$X2.2 == NO | df$X3.2 == YES | df$X3.3 == YES){
    pred_label <- NOT_UNRELATED_BUSINESS_INCOME
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

unrelated_business_income_predict <- function(df){
  
  hardcodes <- unrelated_business_income_logic(df)
  
  if (hardcodes$prediction == UNRELATED_BUSINESS_INCOME |
      hardcodes$prediction == NOT_UNRELATED_BUSINESS_INCOME | 
      hardcodes$prediction == EXCLUDED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(unrelated_business_income_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, UNRELATED_BUSINESS_INCOME, NOT_UNRELATED_BUSINESS_INCOME)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

unrelated_business_income_interim <- function(df){
  
  hardcodes <- unrelated_business_income_logic(df)
  
  if (hardcodes$prediction == UNRELATED_BUSINESS_INCOME |
      hardcodes$prediction == NOT_UNRELATED_BUSINESS_INCOME | 
      hardcodes$prediction == EXCLUDED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(unrelated_business_income_model,
                                    NOT_UNRELATED_BUSINESS_INCOME,
                                    UNRELATED_BUSINESS_INCOME,
                                    data.frame(),
                                    1.5)(df))
  }

}


