prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

gross_negligence_model <- readRDS(prophetPath('models', 'caTaxForesight', 'grossNegligence', 'gn_lr_v1.rds'))

gross_negligence_constants <- data.frame(NOT_GROSSLY_NEGLIGENT='Not grossly negligent',
                                         GROSSLY_NEGLIGENT='Grossly negligent',
                                         stringsAsFactors = FALSE)

gross_negligence_factors <- function(){

  labels <- attributes(gross_negligence_model$terms)$term.labels
  labels <- labels[!(labels %in% c('X3.2divide3.3'))]
  labels <- gsub('^X', '', labels)
  
  labels <- c(labels, c('3.2', '3.3'))
  
  return(labels)
}

gross_negligence_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, gross_negligence_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X1.1', 'X1.3', 'X2.4', 'X3.2', 'X3.3'))
  
  if ("X3.2" %in% answered_questions &
      "X3.3" %in% answered_questions){
    df$X3.2divide3.3 <- df$X3.2 / (df$X3.2 + df$X3.3 + 1)
  }
  
  df <- empty_question_transformation(df, gross_negligence_factors, TRUE, answered_questions)
  
  return(df)
}

gross_negligence_predict <- function(df){
  
  df <- gross_negligence_transform_data(df)
  
  pred_prob <- predict(gross_negligence_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5,
                       gross_negligence_constants$NOT_GROSSLY_NEGLIGENT,
                       gross_negligence_constants$GROSSLY_NEGLIGENT)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

gross_negligence_interim <- function(df){
  
  df <- gross_negligence_transform_data(df)
  
  return(create_interim_predictor(gross_negligence_model,
                                  gross_negligence_constants$GROSSLY_NEGLIGENT,
                                  gross_negligence_constants$NOT_GROSSLY_NEGLIGENT,
                                  data.frame(),
                                  1)(df))
}