prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

primary_beneficiary_model <- readRDS(prophetPath('models', 'caTaxForesight', 'taxableBenefitsPrimaryBeneficiary', 'pb_lr_v1.rds'))

EMPLOYER_PB <- 'Employer'
EMPLOYEE_PB <- 'Employee'

primary_beneficiary_predict <- function(df) {
  
  pred_prob <- predict(primary_beneficiary_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, EMPLOYER_PB, EMPLOYEE_PB)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

primary_beneficiary_interim <- create_interim_predictor(
  primary_beneficiary_model,
  EMPLOYEE_PB,
  EMPLOYER_PB,
  data.frame(),
  0.5
)