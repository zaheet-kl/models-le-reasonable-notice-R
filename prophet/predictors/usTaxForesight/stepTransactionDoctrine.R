prophetSource('interim_analysis', 'interim_analysis.R')

step_transaction_doctrine_model <- readRDS(prophetPath('models', 'usTaxForesight', 'stepTransactionDoctrine', 'step_v1.rds'))

SINGLE_TAXABLE_TRANSACTION <- 'Single Taxable Transaction'
MULTIPLE_TAXABLE_TRANSACTION <- 'Multiple Taxable Transactions'

step_transaction_doctrine_transform_data <- function(df){
  
  df[] <- lapply(df, factor)
  
  return(df)
}

step_transaction_doctrine_predict <- function(df) {
  
  df <- step_transaction_doctrine_transform_data(df)
  
  pred_prob <- predict(step_transaction_doctrine_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, SINGLE_TAXABLE_TRANSACTION, MULTIPLE_TAXABLE_TRANSACTION)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

step_transaction_doctrine_interim <- function(df){
  
  df <- step_transaction_doctrine_transform_data(df)
  
  return(create_interim_predictor(step_transaction_doctrine_model,
                                  MULTIPLE_TAXABLE_TRANSACTION,
                                  SINGLE_TAXABLE_TRANSACTION,
                                  data.frame(),
                                  1)(df))
  
}

