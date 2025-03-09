prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

duty_to_accommodate_disabilities_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'dutyToAccommodateDisabilities', 'duty.rds'))

duty_to_accommodate_disabilities_constants <- data.frame(FULFILLED = 'Employer fulfilled the duty to accommodate',
                                                         DID_NOT_FULFILL = 'Employer did not fulfill the duty to accommodate',
                                                         stringsAsFactors=FALSE)

duty_to_accommodate_disabilities_factors <- function(df){
  
  labels <- attributes(duty_to_accommodate_disabilities_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  return(labels)
}

duty_to_accommodate_disabilities_transform_data <- function(df){
  
  return(df)
  
}

duty_to_accommodate_disabilities_predict <- function(df){
  
  df <- duty_to_accommodate_disabilities_transform_data(df)
  
  pred_prob <- predict(duty_to_accommodate_disabilities_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5,
                       duty_to_accommodate_disabilities_constants$FULFILLED,
                       duty_to_accommodate_disabilities_constants$DID_NOT_FULFILL)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

duty_to_accommodate_disabilities_interim <- function(df){
  
  df <- duty_to_accommodate_disabilities_transform_data(df)
  
  return(create_interim_predictor(duty_to_accommodate_disabilities_model,
                                  duty_to_accommodate_disabilities_constants$DID_NOT_FULFILL,
                                  duty_to_accommodate_disabilities_constants$FULFILLED,
                                  data.frame(),1)(df))
}
