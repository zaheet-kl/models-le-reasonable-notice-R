prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

business_vs_property_model <- readRDS(prophetPath('models', 'caTaxForesight', 'businessVsProperty', 'bvp_lr_v1.rds'))

BUSINESS_BVP <- 'Business income'
PROPERTY_BVP <- 'Property income'

business_vs_property_factors <- function(){
  
  labels <- attributes(business_vs_property_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  
  return(labels)
}

business_vs_property_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, business_vs_property_factors, FALSE, answered_questions)
  
  continuous_vars <- c('X2.2', 'X3.2')
  df <- format_dataframe_numerics(df, continuous_vars)
  
  df <- empty_question_transformation(df, business_vs_property_factors, TRUE, answered_questions)
  
  return(df)
}


business_vs_property_predict <- function(df) {
  
  df <- business_vs_property_transform_data(df)
  
  pred_prob <- predict(business_vs_property_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, PROPERTY_BVP, BUSINESS_BVP)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

business_vs_property_interim <- function(df){
  
  df <- business_vs_property_transform_data(df)
  
  return(create_interim_predictor(business_vs_property_model,
                                  BUSINESS_BVP,
                                  PROPERTY_BVP,
                                  data.frame(),1)(df))
}
