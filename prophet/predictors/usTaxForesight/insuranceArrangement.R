prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

insurance_arrangement_model <- readRDS(prophetPath('models', 'usTaxForesight', 'insuranceArrangement', 'ia_lr_v7.rds'))

NO_INSURANCE_ARRANGEMENT <- 'No Insurance Arrangement'
INSURANCE_ARRANGEMENT <- 'Insurance Arrangement'

YES <- 'Yes'
NO <- 'No'

insurance_arrangement_factors <- function(){
  
  labels <- attributes(insurance_arrangement_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('3.3', '3.3.1')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}

insurance_arrangement_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, insurance_arrangement_factors, FALSE, answered_questions)
  
  if (df$X3.19 == NO | (df$X3.3 == YES & df$X3.3.1 == NO)){
    pred_label <- NO_INSURANCE_ARRANGEMENT
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

insurance_arrangement_predict <- function(df){

  hardcodes <- insurance_arrangement_logic(df)
  
  if (hardcodes$prediction == INSURANCE_ARRANGEMENT |
      hardcodes$prediction == NO_INSURANCE_ARRANGEMENT){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(insurance_arrangement_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, NO_INSURANCE_ARRANGEMENT, INSURANCE_ARRANGEMENT)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

insurance_arrangement_interim <- function(df){

  hardcodes <- insurance_arrangement_logic(df)
  
  if (hardcodes$prediction == INSURANCE_ARRANGEMENT |
      hardcodes$prediction == NO_INSURANCE_ARRANGEMENT){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(insurance_arrangement_model,
                                    INSURANCE_ARRANGEMENT,
                                    NO_INSURANCE_ARRANGEMENT,
                                    data.frame(),
                                    2)(df))
  }
  
}
