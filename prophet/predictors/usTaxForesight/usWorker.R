prophetSource('interim_analysis', 'interim_analysis.R')

us_worker_model <- readRDS(prophetPath('models', 'usTaxForesight', 'usWorker', 'usw_lr_v2.rds'))

INDEPENDENT_CONTRACTOR <- 'Independent contractor'
EMPLOYEE <- 'Employee'

us_worker_predict <- function(df) {
  
  df[] <- lapply(df, factor)
  
  pred_prob <- predict(us_worker_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, INDEPENDENT_CONTRACTOR, EMPLOYEE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

us_worker_interim <- function(df){
  
  df[] <- lapply(df, factor)
  
  return(create_interim_predictor(us_worker_model,
                                  EMPLOYEE,
                                  INDEPENDENT_CONTRACTOR,
                                  data.frame(),
                                  1)(df))
  
}
