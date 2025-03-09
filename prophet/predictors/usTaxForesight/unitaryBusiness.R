prophetSource('interim_analysis', 'interim_analysis.R')

unitary_business_model <- readRDS(prophetPath('models', 'usTaxForesight', 'unitaryBusiness', 'ub_lr_v3.rds'))

unitary_business_constants <- data.frame(YES = 'Yes',
                                         NO = 'No',
                                         ARIZONA = 'Arizona',
                                         STATE_NOT_LISTED_ABOVE = 'A state not listed above',
                                         NO_CG = 'No Combined Group',
                                         SR = 'Separate Reporting',
                                         UB = 'Unitary Business',
                                         NO_UB = 'No Unitary Business',
                                         NO_HARDCODE = '__noHardcode__',
                                         stringsAsFactors = FALSE)

unitary_business_hardcode_locations <- c('X1.2ak', 'X1.2az.2a', 'X1.2az.2b', 'X1.2az.3', 'X1.2az.4', 
                                         'X1.2ca', 'X1.2.co.1', 'X1.2ct', 'X1.2dc', 'X1.2hi', 'X1.2id', 'X1.2il',
                                         'X1.2ks', 'X1.2ky', 'X1.2me', 'X1.2ma', 'X1.2md', 'X1.2mi', 'X1.2mn', 'X1.2mt', 'X1.2ne',
                                         'X1.2nh', 'X1.2nj', 'X1.2nm', 'X1.2ny', 'X1.2nd', 'X1.2or.1a', 'X1.2or.1b', 'X1.2ri', 'X1.2tx',
                                         'X1.2ut', 'X1.2vt', 'X1.2wi', 'X1.2wv')

unitary_business_factors <- function(){
  
  labels <- attributes(unitary_business_model$terms)$term.labels
  labels <- labels[-which(labels %in% c("X3.4Yes.not_arizona", "X3.4No.not_arizona", "X3.4Yes.arizona", "X3.4No.arizona"))]
  labels <- c(labels, c('1.1', '1.3', '3.4', '1.2co.2'))
  
  labels <- c(labels, unitary_business_hardcode_locations)
  labels <- gsub('^X', '', labels)
  
  return(labels)
}

unitary_business_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, unitary_business_factors, FALSE, answered_questions)
  
  if ('X3.4' %in% answered_questions){
    
    df$X3.4Yes.arizona <- factor('false')
    df$X3.4No.arizona <- factor('false')
    df$X3.4Yes.not_arizona <- factor('false')
    df$X3.4No.not_arizona <- factor('false')
    
    if (df$X3.4 == unitary_business_constants$YES & df$X1.1 == unitary_business_constants$ARIZONA){
      df$X3.4Yes.arizona <- factor('true')
    } else if (df$X3.4 == unitary_business_constants$NO & df$X1.1 == unitary_business_constants$ARIZONA){
      df$X3.4No.arizona <- factor('true')
    } else if (df$X3.4 == unitary_business_constants$YES & df$X1.1 != unitary_business_constants$ARIZONA){
      df$X3.4Yes.not_arizona <- factor('true')
    } else if (df$X3.4 == unitary_business_constants$NO & df$X1.1 != unitary_business_constants$ARIZONA){
      df$X3.4No.not_arizona <- factor('true')
    }
    
  }
  
  df <- empty_question_transformation(df, unitary_business_factors, TRUE, answered_questions)
  
  return(df)
}

unitary_business_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, unitary_business_factors, FALSE, answered_questions)
  
  if (df$X1.1 == unitary_business_constants$STATE_NOT_LISTED_ABOVE){
    pred_label <- unitary_business_constants$SR
    pred_prob <- 1
  } else if (any(df[, answered_questions %in% unitary_business_hardcode_locations] == unitary_business_constants$NO)){
    pred_label <- unitary_business_constants$NO_CG
    pred_prob <- 1
  } else if (df$X1.2co.2 == unitary_business_constants$YES |
             df$X1.2tx == unitary_business_constants$YES |
             df$X1.3 == unitary_business_constants$YES){
    pred_label <- unitary_business_constants$UB
    pred_prob <- 1
  } else if (df$X1.2co.2 == unitary_business_constants$NO){
    pred_label <- unitary_business_constants$NO_UB
    pred_prob <- 1 
  } else {
    pred_label <- unitary_business_constants$NO_HARDCODE
    pred_prob <- NA
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

unitary_business_predict <- function(df){
  
  df <- unitary_business_transform_data(df)
  
  hardcodes <- unitary_business_logic(df)
  
  if (hardcodes$prediction == unitary_business_constants$NO_CG |
      hardcodes$prediction == unitary_business_constants$SR |
      hardcodes$prediction == unitary_business_constants$UB |
      hardcodes$prediction == unitary_business_constants$NO_UB){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(unitary_business_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, unitary_business_constants$UB, unitary_business_constants$NO_UB)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

unitary_business_interim <- function(df){
  
  df <- unitary_business_transform_data(df)
  
  hardcodes <- unitary_business_logic(df)
  
  if (hardcodes$prediction == unitary_business_constants$NO_CG |
      hardcodes$prediction == unitary_business_constants$SR |
      hardcodes$prediction == unitary_business_constants$UB |
      hardcodes$prediction == unitary_business_constants$NO_UB){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(unitary_business_model,
                                    unitary_business_constants$NO_UB,
                                    unitary_business_constants$UB,
                                    data.frame(),
                                    3)(df))
  }
  
}

