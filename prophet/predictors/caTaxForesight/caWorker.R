prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

ca_worker_model <- readRDS(prophetPath('models', 'caTaxForesight', 'worker', 'worker_lr_v1.rds'))

CA_INDEPENDENT_CONTRACTOR <- 'Independent Contractor'
CA_EMPLOYEE <- 'Employee'

ca_worker_factors <- function(){
  
  labels <- attributes(ca_worker_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  
  return(labels)
}

ca_worker_transform_data <- function(df){

  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, ca_worker_factors, FALSE, answered_questions)

  # specify the continuous variables, factor variables, and logical variables (the three types used in the model)
  continuous_vars <- c('X1.4')
  factor_vars <- c('X1.3', 'X2.1', 'X2.2', 'X2.3', 'X2.4', 'X2.5', 'X2.6', 'X2.7', 'X2.8', 'X3.2', 'X4.1', 'X4.3', 'X5.1', 'X5.2')

  # format the numeric variables before performaning any transformations
  df <- format_dataframe_numerics(df, continuous_vars)

  # apply the factor function that converts the input to factor type (exclude the continuous and logical variables)
  factor_inds <- which(!(names(df) %in% c(continuous_vars)))
  df[ , factor_inds] <- lapply(df[ , factor_inds], factor)

  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, ca_worker_factors, TRUE, answered_questions)


  return(df)
}

ca_worker_predict <- function(df) {
  
  df <- ca_worker_transform_data(df)
  
  pred_prob <- predict(ca_worker_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, CA_INDEPENDENT_CONTRACTOR, CA_EMPLOYEE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

ca_worker_interim <- function(df){
  
  df <- ca_worker_transform_data(df)
  
  return(create_interim_predictor(ca_worker_model,
                                  CA_EMPLOYEE,
                                  CA_INDEPENDENT_CONTRACTOR,
                                  data.frame(),
                                  5)(df))
  
}

