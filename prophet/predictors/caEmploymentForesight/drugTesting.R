prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

drug_testing_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'drugTesting', 'drug.rds'))

drug_testing_constants <- data.frame(REASONABLE = 'Reasonable testing',
                                                         NOT_REASONABLE = 'Not reasonable testing',
                                                         stringsAsFactors=FALSE)

drug_testing_factors <- function(df){
  
  labels <- attributes(drug_testing_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  return(labels)
}

drug_testing_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, drug_testing_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X2.2', 'X3.3', 'X4.1'))
  
  df <- empty_question_transformation(df, drug_testing_factors, TRUE, answered_questions)
  
  return(df)
  
}

drug_testing_predict <- function(df){
  
  df <- drug_testing_transform_data(df)
  
  pred_prob <- predict(drug_testing_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5,
                       drug_testing_constants$REASONABLE,
                       drug_testing_constants$NOT_REASONABLE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

drug_testing_interim <- function(df){
  
  df <- drug_testing_transform_data(df)
  
  return(create_interim_predictor(drug_testing_model,
                                  drug_testing_constants$NOT_REASONABLE,
                                  drug_testing_constants$REASONABLE,
                                  data.frame(),0.5)(df))
}
