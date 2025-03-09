prophetSource('interim_analysis', 'interim_analysis.R')

ten_thirty_one_model <- readRDS(prophetPath('models', 'usTaxForesight', 'tenThirtyOne', 'tto.rds'))

NOT_HPI <- 'Not held for productive use or investment'
HPI <- 'Held for productive use or investment'

ten_thirty_one_factors <- function(){
  
  labels <- attributes(ten_thirty_one_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  labels <- c(labels, '2.1.02', '2.1.03', '2.1.04')
  
  return(labels)
}

ten_thirty_one_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, ten_thirty_one_factors, FALSE, answered_questions)

  continuous_vars <- c('X4.1')
  factor_vars <- c('X1.2', 'X2.1_customChoiceValue', 'X2.1.01_customChoiceValue',
                   'X3.1', 'X3.1.01_customChoiceValue', 'X3.1.02_customChoiceValue',
                   'X4.2_customChoiceValue', 'X4.3.01', 'X4.4.01_customChoiceValue', 'X4.5', 'X4.6_customChoiceValue', 'X4.7_customChoiceValue')
  hardcode_vars <- c('X3.1.01_customChoiceValue', 'X2.1.02', 'X2.1.03', 'X2.1.04')
  
  factor_inds <- which(!(names(df) %in% c(continuous_vars, hardcode_vars)))
  continuous_inds <- which(!(names(df) %in% c(factor_vars, hardcode_vars)))
  
  df[ , factor_inds] <- lapply(df[ , factor_inds], factor)
  df[ , continuous_inds] <- lapply(df[ , continuous_inds], as.numeric)

  df <- empty_question_transformation(df, ten_thirty_one_factors, TRUE, answered_questions)
  
  return(df)
}

ten_thirty_one_logic <- function(df) {
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, ten_thirty_one_factors, FALSE, answered_questions)
  
  if(df$X3.1.01_customChoiceValue == 'inventory'){
    pred_prob <- 1
    pred_label <- NOT_HPI
  } else if(df$X2.1.02 == 'Yes' |
            df$X2.1.03 == 'Yes' |
            df$X2.1.04 == 'Yes') {
    pred_prob <- 0
    pred_label <- HPI
  } else {
    pred_prob <- NA
    pred_label <- '__noHardcode__'
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

ten_thirty_one_predict <- function(df) {
  
  df <- ten_thirty_one_transform_data(df)
  
  hardcodes <- ten_thirty_one_logic(df)
  
  if(hardcodes$prediction == HPI |
     hardcodes$prediction == NOT_HPI){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(ten_thirty_one_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, NOT_HPI, HPI)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

ten_thirty_one_interim <- function(df){
  
  df <- ten_thirty_one_transform_data(df)
  
  hardcodes <- ten_thirty_one_logic(df)
  
  if(hardcodes$prediction == HPI |
     hardcodes$prediction == NOT_HPI){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(ten_thirty_one_model,
                                    HPI,
                                    NOT_HPI,
                                    data.frame(),
                                    1)(df))
  }
}