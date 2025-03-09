prophetSource('interim_analysis', 'interim_analysis.R')

intangible_expenditure_model <- readRDS(prophetPath('models', 'caTaxForesight', 'intangibleExpenditure', 'ie_lr_v1.rds'))

CURRENT_INT_EXP <- 'Current'
CAPITAL_INT_EXP <- 'Capital'

intangible_expenditure_factors <- function(){
  labels <- attributes(intangible_expenditure_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  
  return(labels)  
}

intangible_expenditure_transform_data <- function(df){
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, intangible_expenditure_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X2.3'))
  
  df <- empty_question_transformation(df, intangible_expenditure_factors, TRUE, answered_questions)
  
  return(df)
}

intangible_expenditure_predict <- function(df) {
  
  df <- intangible_expenditure_transform_data(df)
  
  pred_prob <- predict(intangible_expenditure_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, CURRENT_INT_EXP, CAPITAL_INT_EXP)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

intangible_expenditure_interim <- function(df){
  
  df <- intangible_expenditure_transform_data(df)
  
  return(create_interim_predictor(intangible_expenditure_model,
                                  CAPITAL_INT_EXP,
                                  CURRENT_INT_EXP,
                                  data.frame(),1)(df))
}
