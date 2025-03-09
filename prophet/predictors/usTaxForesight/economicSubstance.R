prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

economic_substance_model <- readRDS(prophetPath('models', 'usTaxForesight', 'economicSubstance', 'es_lr_v1.rds'))

SUBSTANCE <- 'Economic substance'
NO_SUBSTANCE <- 'No economic substance'
NOPE <- 'No'
YES <- 'Yes'

economic_substance_factors <- function(){
  
  return(c(
    "1.0.1_customChoiceValue",
    "1.1",
    "1.2",
    "1.3",
    "3.5.1",
    "3.5.2",
    "1.5",
    "1.6",
    "1.7",
    "2.3.1",
    "2.4.1",
    "2.8",
    "3.6",
    "3.7",
    "4.1",
    "4.2",
    "4.3",
    "4.4",
    "4.5",
    "4.6",
    "4.7",
    "3.1",
    "3.3",
    "3.4",
    "3.7.1",
    "5.1",
    "5.2.1",
    "5.2.2"   
  ))
}

economic_substance_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, economic_substance_factors, FALSE, answered_questions)

  df <- format_dataframe_numerics(df, c('X1.7', 'X3.1', 'X3.3', 'X3.4', 'X3.7.1', 'X5.1', 'X5.2.1', 'X5.2.2'))
  
  if(is.na(df$X5.1)){
  } else{
    df$ROI <- df$X3.7.1 / (1 + df$X3.7.1 + df$X5.1 + df$X5.2.1)
    
    df$LossToProfit <- df$X3.1 / (1 + df$X3.1 + df$X3.7.1)
    
    df$SavingsToProfit <- df$X3.3 / (1 + df$X3.3 + df$X3.7.1)
    
    df$SavingsToRisk <- df$X3.3 / (1 + df$X3.3 + df$X5.1 + df$X5.2.1)
    
    df$CreditsToProfit <- df$X3.4 / (1 + df$X3.4 + df$X3.7.1)
    
    df$CreditsToRisk <- df$X3.4 / (1 + df$X3.4 + df$X5.1 + df$X5.2.1)
    
    df$NoRiskToRisk <- df$X5.2.2 / (1 + df$X5.2.2 + df$X5.1 + df$X5.2.1)
  }
  
  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, economic_substance_factors, TRUE, answered_questions)
  
  return(df)
}

economic_substance_predict <- function(df){
  
  df <- economic_substance_transform_data(df)
  
  pred_prob <- predict(economic_substance_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, NO_SUBSTANCE, SUBSTANCE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
  
}

economic_substance_interim <- function(df){
  
  df <- economic_substance_transform_data(df)
  
  return(create_interim_predictor(economic_substance_model,
                                  SUBSTANCE,
                                  NO_SUBSTANCE,
                                  data.frame(),
                                  1)(df))
  
}