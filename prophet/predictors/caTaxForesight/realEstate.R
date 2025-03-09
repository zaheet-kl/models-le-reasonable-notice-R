prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

real_estate_ca_model <- readRDS(prophetPath('models', 'caTaxForesight', 'realEstate', 'realestate_lr_v1.rds'))

real_estate_ca_constants <- data.frame(CAPITAL = 'Capital',
                                       BUSINESS = 'Business',
                                       stringsAsFactors = FALSE)

real_estate_ca_factors <- function(){
  
  labels <- attributes(real_estate_ca_model$terms)$term.labels
  labels <- labels[!(labels %in% c('X5.2log', 'X2.6log', 'X4.2log'))]
  labels <- gsub('^X', '', labels)
  
  labels <- c(labels, '5.2', '2.6', '4.2')
  
  return(labels)
}

real_estate_ca_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, real_estate_ca_factors, FALSE, answered_questions)

  # specify the continuous variables, factor variables, and logical variables (the three types used in the model)
  continuous_vars <- c('X2.6', 'X3.4', 'X4.2', 'X5.2')
  
  # format the numeric variables before performing any transformations
  df <- format_dataframe_numerics(df, continuous_vars)
  
  if ("X5.2" %in% answered_questions){
    df$X5.2log <- log(df$X5.2 + 1)
  }
  
  if ("X2.6" %in% answered_questions){
    df$X2.6log <- log(df$X2.6 + 1)
  }
  
  if ("X4.2" %in% answered_questions){
    df$X4.2log <- log(df$X4.2 + 1)
  }

  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, real_estate_ca_factors, TRUE, answered_questions)
  
  return(df)
}

real_estate_ca_predict <- function(df){
  
  df <- real_estate_ca_transform_data(df)
  
  pred_prob <- predict(real_estate_ca_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5, real_estate_ca_constants$CAPITAL, real_estate_ca_constants$BUSINESS)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

real_estate_ca_interim <- function(df){
  
  df <- real_estate_ca_transform_data(df)
  
  return(create_interim_predictor(real_estate_ca_model,
                                  real_estate_ca_constants$BUSINESS,
                                  real_estate_ca_constants$CAPITAL,
                                  data.frame(),
                                  2)(df))
}
