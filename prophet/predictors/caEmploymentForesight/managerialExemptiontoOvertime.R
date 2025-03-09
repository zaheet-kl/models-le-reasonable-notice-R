prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

managerial_overtime_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'managerialExemptiontoOvertime', 'meo_lr_v1.rds'))

managerial_overtime_constants <- data.frame(NOT_A_MANAGER = 'Not a manager',
                            MANAGERIAL_IN_CHARACTER = 'Managerial in character',
                            stringsAsFactors = FALSE)

managerial_overtime_factors <- function(){
  
  labels <- attributes(managerial_overtime_model$terms)$term.labels
  labels <- labels[!(labels %in% c('X1.1Quebec'))]
  labels <- gsub('^X', '', labels)
  
  labels <- c(labels, '1.1')
  
  return(labels)
}

managerial_overtime_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, managerial_overtime_factors, FALSE, answered_questions)
  
  # create quebec variable for 1.1
  df$X1.1Quebec <- ifelse(grepl('Quebec', df$X1.1), TRUE, FALSE)
  
  # specify the continuous variables, factor variables, and logical variables (the three types used in the model)
  continuous_vars <- c('X3.3', 'X3.4')
  
  # format the numeric variables before performing any transformations
  df <- format_dataframe_numerics(df, continuous_vars)
  
  return(df)
}

managerial_overtime_predict <- function(df){
  
  df <- managerial_overtime_transform_data(df)
  
  pred_prob <- predict(managerial_overtime_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5, managerial_overtime_constants$NOT_A_MANAGER, managerial_overtime_constants$MANAGERIAL_IN_CHARACTER)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

managerial_overtime_interim <- function(df){
  
  df <- managerial_overtime_transform_data(df)
  
  return(create_interim_predictor(managerial_overtime_model,
                                  managerial_overtime_constants$MANAGERIAL_IN_CHARACTER,
                                  managerial_overtime_constants$NOT_A_MANAGER,
                                  data.frame(),
                                  3)(df))
}
