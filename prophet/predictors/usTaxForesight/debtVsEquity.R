prophetSource('interim_analysis', 'interim_analysis.R')

debt_vs_equity_model <- readRDS(prophetPath('models', 'usTaxForesight', 'debtVsEquity', 'dve_lr_v2.rds'))

EQUITY <- 'Equity'
DEBT <- 'Debt'

debt_vs_equity_transform_data <- function(df){
  
  df$X3.3 <- plyr::revalue(df$X3.3, c("Not applicable"="No - the amounts were not proportional"))
  
  return(df)
}

debt_vs_equity_predict <- function(df) {
  
  df <- debt_vs_equity_transform_data(df)
  
  pred_prob <- predict(debt_vs_equity_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, EQUITY, DEBT)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

debt_vs_equity_interim <- function(df){
  
  df <- debt_vs_equity_transform_data(df)
  
  return(create_interim_predictor(debt_vs_equity_model,
                                  DEBT,
                                  EQUITY,
                                  data.frame(),
                                  1)(df))
  
}


