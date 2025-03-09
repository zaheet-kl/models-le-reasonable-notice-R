prophetSource('interim_analysis', 'interim_analysis.R')

trust_fund_recovery_penalty_model <- readRDS(prophetPath('models', 'usTaxForesight', 'trustFundRecoveryPenalty', 'tfrp_lr_v1.rds'))

TFRP <- 'TFRP'
NO_TFRP <- 'No TFRP'

trust_fund_recovery_penalty_predict <- function(df) {
  
  pred_prob <- predict(trust_fund_recovery_penalty_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, TFRP, NO_TFRP)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

trust_fund_recovery_penalty_interim <- create_interim_predictor(
  trust_fund_recovery_penalty_model,
  NO_TFRP,
  TFRP,
  data.frame(),
  1
)