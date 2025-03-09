prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

ca_emp_worker_simple_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'caEmpWorker', 'ca_worker_simple_lr_v1.rds'))
ca_emp_worker_step1_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'caEmpWorker', 'ca_worker_step1_lr_v1.rds'))
ca_emp_worker_step2_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'caEmpWorker', 'ca_worker_step2_lr_v1.rds'))


ca_emp_worker_constants <- data.frame(INDEPENDENT_CONTRACTOR = 'Independent contractor',
                                  EMPLOYEE = 'Employee',
                                  DEPENDENT_CONTRACTOR = 'Dependent contractor',
                                  stringsAsFactors = FALSE)

ca_emp_worker_factors <- function(){
  
  labels_simple <- attributes(ca_emp_worker_simple_model$terms)$term.labels
  labels_step1 <- attributes(ca_emp_worker_step1_model$terms)$term.labels
  labels_step2 <- attributes(ca_emp_worker_step2_model$terms)$term.labels
  
  labels <- c(labels_simple, labels_step1, labels_step2, 'X6.1', 'X6.2', 'X6.3')
  
  labels <- unique(labels)
  
  labels <- gsub('^X', '', labels)
  
  return(labels)
}

ca_emp_worker_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, ca_emp_worker_factors, FALSE, answered_questions)
  
  # specify the continuous variables, factor variables, and logical variables (the three types used in the model)
  continuous_vars <- c('X1.5', 'X4.1', 'X4.2')
  
  # format the numeric variables before performing any transformations
  df <- format_dataframe_numerics(df, continuous_vars)
  
  df <- empty_question_transformation(df, ca_emp_worker_factors, TRUE, answered_questions)
  
  return(df)
}

ca_emp_worker_predict <- function(df){
  
  province_list <- c('British Columbia', 'Saskatchewan', 'Ontario', 'New Brunswick', 'Federal', 'Newfoundland and Labrador')
  
  df <- ca_emp_worker_transform_data(df)
  
  if(df$X6.1 == 'Yes' | (df$X6.2 == 'Yes' & df$X6.3 %in% province_list)) {
    #Step 1 prediction
    step1_pred_prob <- predict(ca_emp_worker_step1_model, newdata=df, type='response')
    pred_label <- ifelse(step1_pred_prob>0.5, ca_emp_worker_constants$INDEPENDENT_CONTRACTOR, ca_emp_worker_constants$EMPLOYEE)
  
    if(pred_label == ca_emp_worker_constants$INDEPENDENT_CONTRACTOR) {
      #Step 2 prediction
      step2_pred_prob <- predict(ca_emp_worker_step2_model, newdata=df, type='response')
      pred_label <- ifelse(step2_pred_prob>0.5, ca_emp_worker_constants$INDEPENDENT_CONTRACTOR, ca_emp_worker_constants$DEPENDENT_CONTRACTOR)

       return(list(
        prediction=pred_label,
        probability=(convert_probability_above_50(step1_pred_prob) + convert_probability_above_50(step2_pred_prob)) / 2
      ))
    }
    
    return(list(
      prediction=pred_label,
      probability=step1_pred_prob
    ))
    
  }
  
  #Simple prediction
  pred_prob <- predict(ca_emp_worker_simple_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5, ca_emp_worker_constants$INDEPENDENT_CONTRACTOR, ca_emp_worker_constants$EMPLOYEE)

  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
  
}

ca_emp_worker_interim <- function(df){

  province_list <- c('British Columbia', 'Saskatchewan', 'Ontario', 'New Brunswick', 'Federal', 'Newfoundland and Labrador')

  df <- ca_emp_worker_transform_data(df)

  if(df$X6.1 == 'Yes' | (df$X6.2 == 'Yes' & df$X6.3 %in% province_list)) {
    #Step 1 interim
    interim_worker <- create_interim_predictor(ca_emp_worker_step1_model,
                             ca_emp_worker_constants$EMPLOYEE,
                             ca_emp_worker_constants$INDEPENDENT_CONTRACTOR,
                             data.frame(),
                             1)(df)


    if(pred_label == ca_emp_worker_constants$INDEPENDENT_CONTRACTOR) {
      #Step 2 prediction
      interim_worker <- create_interim_predictor(ca_emp_worker_step2_model,
                                                 ca_emp_worker_constants$DEPENDENT_CONTRACTOR,
                                                 ca_emp_worker_constants$INDEPENDENT_CONTRACTOR,
                                                 data.frame(),
                                                 1)(df)

    }

    return(interim_worker)

  }

  #Simple prediction
  return(create_interim_predictor(ca_emp_worker_simple_model,
                                  ca_emp_worker_constants$EMPLOYEE,
                                  ca_emp_worker_constants$INDEPENDENT_CONTRACTOR,
                                  data.frame(),
                                  1)(df))
}
