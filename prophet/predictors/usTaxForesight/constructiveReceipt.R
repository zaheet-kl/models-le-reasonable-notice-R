prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

constructive_receipt_model <- readRDS(prophetPath('models', 'usTaxForesight', 'constructiveReceipt', 'cr_lr_v8.rds'))

NO_CONSTRUCTIVE_RECEIPT <- 'No Constructive Receipt of Income'
CONSTRUCTIVE_RECEIPT <- 'Constructive Receipt of Income'

constructive_receipt_factors <- function(){
  
  labels <- attributes(constructive_receipt_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  labels <- c(labels, '4.9')
  
  return(labels)
}

constructive_receipt_logic <- function(df){
  
  answered_questions <- names(df)
  df <- empty_question_transformation(df, constructive_receipt_factors, FALSE, answered_questions)
  
  if (df$X4.9 == 'No'){
    pred_label <- CONSTRUCTIVE_RECEIPT
    pred_prob <- 0
  } else {
    pred_label <- '__noHardcode__'
    pred_prob <- NA
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}



constructive_receipt_predict <- function(df) {
  
  df[ , !(names(df) %in% c('X3.1'))] <- lapply(df[ , !(names(df) %in% c('X3.1'))], factor)
  
  hardcodes <- constructive_receipt_logic(df)
  
  if (hardcodes$prediction == CONSTRUCTIVE_RECEIPT){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(constructive_receipt_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, NO_CONSTRUCTIVE_RECEIPT, CONSTRUCTIVE_RECEIPT)
    
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

constructive_receipt_interim <- function(df){
  
  df[ , !(names(df) %in% c('X3.1'))] <- lapply(df[ , !(names(df) %in% c('X3.1'))], factor)
  
  
  hardcodes <- constructive_receipt_logic(df)
  
  if (hardcodes$prediction == CONSTRUCTIVE_RECEIPT){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
    
  } else {
    return(create_interim_predictor(constructive_receipt_model,
                                    CONSTRUCTIVE_RECEIPT,
                                    NO_CONSTRUCTIVE_RECEIPT,
                                    data.frame(),
                                    2.5)(df))
  }
}
