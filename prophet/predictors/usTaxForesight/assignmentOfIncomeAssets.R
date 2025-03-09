prophetSource('interim_analysis', 'interim_analysis.R')

assignment_of_income_assets_model <- readRDS(prophetPath('models', 'usTaxForesight', 'assignmentOfIncomeAssets', 'aoia_lr_v5.rds'))

TRANSFEROR_IS_LIABLE <- 'Transferor is liable'
TRANSFEREE_IS_LIABLE <- 'Transferee is liable'

assignment_of_income_assets_predict <- function(df) {
  
  df[] <- lapply(df, factor)
  
  pred_prob <- predict(assignment_of_income_assets_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, TRANSFEROR_IS_LIABLE, TRANSFEREE_IS_LIABLE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

assignment_of_income_assets_interim <- function(df){
  
  df[] <- lapply(df, factor)
  
  return(create_interim_predictor(assignment_of_income_assets_model,
                                  TRANSFEREE_IS_LIABLE,
                                  TRANSFEROR_IS_LIABLE,
                                  data.frame(),
                                  1)(df))
  
}
