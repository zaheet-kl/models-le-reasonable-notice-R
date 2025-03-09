prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

windfall_model <- readRDS(prophetPath('models', 'caTaxForesight', 'windfall', 'win_lr_v1.rds'))

windfall_constants <- data.frame(NOT_BUSINESS = 'The gambling activity is not a business',
                                 BUSINESS = 'The gambling activity is a business',
                                 stringsAsFactors = FALSE)

windfall_factors <- function(){
  labels <- attributes(windfall_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  
  return(labels)  
}

windfall_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, windfall_factors, FALSE, answered_questions)
  
  # specify the continuous variables, factor variables, and logical variables (the three types used in the model)
  continuous_vars <- c('X1.3', 'X2.2', 'X2.3', 'X3.1')
  factor_vars <- c('X1.4', 'X1.5', 'X2.1', 'X2.4', 'X2.5', 'X2.6', 'X3.2')
  
  # format the numeric variables before performaning any transformations
  df <- format_dataframe_numerics(df, continuous_vars)
  
  # apply the factor function that converts the input to factor type (exclude the continuous and logical variables)
  factor_inds <- which(!(names(df) %in% c(continuous_vars)))
  df[ , factor_inds] <- lapply(df[ , factor_inds], factor)
  
  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, windfall_factors, TRUE, answered_questions)
  
  
  return(df)
}

windfall_predict <- function(df){
  
  df <- windfall_transform_data(df)
  
  pred_prob <- predict(windfall_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5, windfall_constants$NOT_BUSINESS, windfall_constants$BUSINESS)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

windfall_interim <- function(df){
  
  df <- windfall_transform_data(df)
  
  return(create_interim_predictor(windfall_model,
                                  windfall_constants$BUSINESS,
                                  windfall_constants$NOT_BUSINESS,
                                  data.frame(),
                                  1)(df))
}

