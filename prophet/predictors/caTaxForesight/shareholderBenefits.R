prophetSource('interim_analysis', 'interim_analysis.R')

shareholder_benefits_model <- readRDS(prophetPath('models', 'caTaxForesight', 'shareholderBenefits', 'sb_lr_v1.rds'))

SHAREHOLDER_BENEFIT <- 'Shareholder benefit'
NOT_A_SHAREHOLDER_BENEFIT <- 'Not a shareholder benefit'

shareholder_benefits_predict <- function(df) {
  
  pred_prob <- predict(shareholder_benefits_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, SHAREHOLDER_BENEFIT, NOT_A_SHAREHOLDER_BENEFIT)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

shareholder_benefits_interim <- create_interim_predictor(
  shareholder_benefits_model,
  NOT_A_SHAREHOLDER_BENEFIT,
  SHAREHOLDER_BENEFIT,
  data.frame(),
  2
)
