prophetSource('interim_analysis', 'interim_analysis.R')

continuity_of_business_enterprise_model <- readRDS(prophetPath('models', 'usTaxForesight', 'continuityOfBusinessEnterprise', 'cobe_v10.rds'))

NO_CONTINUITY_OF_BUSINESS_ENTERPRISE <- 'No continuity of business enterprise'
CONTINUITY_OF_BUSINESS_ENTERPRISE <- 'Continuity of business enterprise'

continuity_of_business_enterprise_predict <- function(df) {
  
  df[] <- lapply(df, factor)
  
  pred_prob <- predict(continuity_of_business_enterprise_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, NO_CONTINUITY_OF_BUSINESS_ENTERPRISE, CONTINUITY_OF_BUSINESS_ENTERPRISE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

continuity_of_business_enterprise_interim <- function(df){
  
  df[] <- lapply(df, factor)
  
  return(create_interim_predictor(continuity_of_business_enterprise_model,
                                  CONTINUITY_OF_BUSINESS_ENTERPRISE,
                                  NO_CONTINUITY_OF_BUSINESS_ENTERPRISE,
                                  data.frame(),
                                  1)(df))
  
}
  

