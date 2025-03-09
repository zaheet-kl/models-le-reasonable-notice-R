prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

real_estate_us_model <- readRDS(prophetPath('models', 'usTaxForesight', 'realEstateUS', 'reus.rds'))

NOT_CAPITAL_US_REAL_ESTATE <- 'Not a capital asset'
CAPITAL_US_REAL_ESTATE <- 'Capital asset'

real_estate_us_factors <- function(){
  
  return(c(
    "1.0.1_customChoiceValue",
    "3.4",
    "4.1",
    "4.3",
    "5.2",
    "5.3",
    "6.1",
    "6.2",
    "6.3",
    "6.4",
    "6.5",
    "7.1",
    "7.2",
    "7.3",
    "8.1",
    "8.3",
    "3.5",
    "8.6",
    "8.5"    
  ))
}

real_estate_us_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, real_estate_us_factors, FALSE, answered_questions)
  
  df[] <- lapply(df, factor)
  
  df <- format_dataframe_numerics(df, c('X4.1', 'X7.1', 'X7.2', 'X7.3', 'X8.1'))
  
  if(df$X7.1 == '__unanswered__'){
    df$X7.1log <- 0
    df$X7.2log <- 0
    df$X7.3log <- 0
    df$X8.1log <- 0
  } else{
    df$X7.1log <- log(1 + df$X7.1)
    df$X7.2log <- log(1 + df$X7.2)
    df$X7.3log <- log(1 + df$X7.3)
    df$X8.1log <- log(1 + df$X8.1)
  }
  
  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, real_estate_us_factors, TRUE, answered_questions)
  
  return(df)
}

real_estate_us_predict <- function(df){
  
  df <- real_estate_us_transform_data(df)
  
  pred_prob <- predict(real_estate_us_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, NOT_CAPITAL_US_REAL_ESTATE, CAPITAL_US_REAL_ESTATE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
  
}

real_estate_us_interim <- function(df){
  
  df <- real_estate_us_transform_data(df)
  
  return(create_interim_predictor(real_estate_us_model,
                                  CAPITAL_US_REAL_ESTATE,
                                  NOT_CAPITAL_US_REAL_ESTATE,
                                  data.frame(),
                                  1)(df))
  
}