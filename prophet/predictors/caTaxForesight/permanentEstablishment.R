prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

permanent_establishment_fpob_model <- readRDS(prophetPath('models', 'caTaxForesight', 'permanentEstablishment', 'pe_fpob_lr_v1.rds'))
permanent_establishment_da_model <- readRDS(prophetPath('models', 'caTaxForesight', 'permanentEstablishment', 'pe_da_lr_v1.rds'))

permanent_establishment_constants <- data.frame(PERMANENT_ESTABLISHMENT='Permanent establishment',
                                                NO_PERMANENT_ESTABLISHMENT='No permanent establishment',
                                                FPOB_PE='FPOB PE',
                                                NO_FPOB_PE='No FPOB PE',
                                                DEPENDENT_AGENT='Dependent Agent',
                                                INDEPENDENT_AGENT='Independent Agent',
                                                DA_PE='DA PE',
                                                NO_DA_PE='No DA PE',
                                                SERVICES_PE='Services PE',
                                                NO_SERVICES_PE='No Services PE',
                                                YES='Yes',
                                                NO='No',
                                                SALE_OR_LEASE_OF_PROPERTY='The sale or lease of personal property, manufactured or purchased, tangible or intangible in Canada',
                                                PERFORMANCE_OF_SERVICES='The performance of services in Canada',
                                                stringsAsFactors=FALSE)


permanent_establishment_factors <- function(df){
  
  labels_fpob <- attributes(permanent_establishment_fpob_model$terms)$term.labels
  labels_fpob <- gsub('^X', '', labels_fpob)
  labels_fpob <- labels_fpob[-which(c('1.4log', '1.5log') %in% labels_fpob)]
  labels_fpob <- c(labels_fpob, c('1.4', '1.5'))
  
  
  labels_da <- attributes(permanent_establishment_da_model$terms)$term.labels
  labels_da <- gsub('^X', '', labels_da)
  
  hardcode_factors <- c('1.1', '2.2new', '2.1', '2.3', '3.2new', '2.11', '4.2', '4.3', '4.4', '4.1.1', '4.1.2', '4.4',
                        '1.1.1a', '1.1.1b')
  
  ml_condition_factors <- c('1.1new')
  
  factors <- c(labels_fpob, labels_da, hardcode_factors, ml_condition_factors)
  return(factors)
}

permanent_establishment_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors, FALSE, answered_questions)

  df <- format_dataframe_numerics(df, c('X1.4', 'X1.5'))
  
  if (is.na(df$X1.4) | is.na(df$X1.5)){
    df$X1.4log <- 0
    df$X1.5log <- 0
  } else{
    df$X1.4log <- log(df$X1.4)
    df$X1.5log <- log(df$X1.5)
  }
  
  df <- empty_question_transformation(df, permanent_establishment_factors, TRUE, answered_questions)
  
  return(df)
}

permanent_establishment_fpob_logic <- function(df, fpob_prob, fpob_label){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors, FALSE, answered_questions)
  
  if (df$X1.1 == permanent_establishment_constants$NO | df$X2.2 == permanent_establishment_constants$YES){
    fpob_label <- permanent_establishment_constants$NO_FPOB_PE
    fpob_prob <- 1
  }
  
  return(list(
    subresult_1_label=fpob_label,
    subresult_1_prob=fpob_label
  ))
}

permanent_establishment_da_logic <- function(df, da_prob, da_label){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors, FALSE, answered_questions)
  
  if ((df$X2.1 == permanent_establishment_constants$NO | df$X2.3 == permanent_establishment_constants$NO | df$X3.2new == permanent_establishment_constants$YES) |
      (da_label == permanent_establishment_constants$INDEPENDENT_AGENT & df$X2.11 == permanent_establishment_constants$YES)){
      subresult_2_label <- permanent_establishment_constants$NO_DA_PE
      subresult_2_prob <- 1
  } else if (da_label == permanent_establishment_constants$INDEPENDENT_AGENT & df$X2.11 == permanent_establishment_constants$NO){
    subresult_2_label <- permanent_establishment_constants$DA_PE
    subresult_2_prob <- 1
  } else {
    subresult_2_label <- permanent_establishment_constants$DA_PE
    subresult_2_prob <- da_prob
  }
  
  return(list(
    subresult_2_label=subresult_2_label,
    subresult_2_prob=subresult_2_prob
  ))
}

permanent_establishment_services_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors, FALSE, answered_questions)
  
  if (df$X4.2 == permanent_establishment_constants$NO | df$X4.3 == permanent_establishment_constants$NO | df$X4.4 == permanent_establishment_constants$NO){
    subresult_3_label <- permanent_establishment_constants$NO_SERVICES_PE
    subresult_3_prob <- 1
  } else if (df$X4.1.1 == permanent_establishment_constants$YES | df$X4.1.2 == permanent_establishment_constants$YES | df$X4.4 == permanent_establishment_constants$YES){
    subresult_3_label <- permanent_establishment_constants$SERVICES_PE
    subresult_3_prob <- 1
  } else {
    subresult_3_label <- permanent_establishment_constants$NO_SERVICES_PE
    subresult_3_prob <- 1
  }
  
  return(list(
    subresult_3_label=subresult_3_label,
    subresult_3_prob=subresult_3_prob
  ))
}

da_model_use <- function(df){
  
  if ((df$X1.1new == permanent_establishment_constants$SALE_OR_LEASE_OF_PROPERTY | df$X1.1new == permanent_establishment_constants$PERFORMANCE_OF_SERVICES) &
      df$X2.1 == permanent_establishment_constants$YES & 
      df$X3.2new == permanent_establishment_constants$YES &
      df$X2.3 == permanent_establishment_constants$YES){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

fpob_model_use <- function(df){
  
  if ((df$X1.1new == permanent_establishment_constants$SALE_OR_LEASE_OF_PROPERTY | df$X1.1new == permanent_establishment_constants$PERFORMANCE_OF_SERVICES) &
      df$X1.1 == permanent_establishment_constants$YES &
      df$X2.2new == permanent_establishment_constants$NO){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

permanent_establishment_predict <- function(df){
  
  df <- permanent_establishment_transform_data(df)
  
  if (fpob_model_use(df)){
    fpob_prob <- predict(permanent_establishment_fpob_model, df, type = 'response')
    fpob_label <- ifelse(fpob_prob > 0.5, permanent_establishment_constants$NO_FPOB_PE, permanent_establishment_constants$FPOB_PE)
  } else {
    fpob_prob <- 0.5
    fpob_label <- permanent_establishment_constants$NO_FPOB_PE
  }
  
  fpob_results <- permanent_establishment_da_logic(df, fpob_prob, fpob_label)
  subresult_1_prob <- fpob_results$subresult_1_prob
  subresult_1_label <- fpob_results$subresult_1_label
  
  if (da_model_use(df)){
    da_prob <- predict(permanent_establishment_da_model, df, type = 'response')
    da_label <- ifelse(da_prob > 0.5, permanent_establishment_constants$INDEPENDENT_AGENT, permanent_establishment_constants$DEPENDENT_AGENT)
  } else {
    da_prob <- 0.5
    da_label <- permanent_establishment_constants$INDEPENDENT_AGENT
  }
  
  da_results <- permanent_establishment_da_logic(df, da_prob, da_label)
  subresult_2_prob <- da_results$subresult_2_prob
  subresult_2_label <- da_results$subresult_2_label
  
  services_results <- permanent_establishment_services_logic(df)
  subresult_3_prob <- services_results$subresult_3_prob
  subresult_3_label <- services_results$subresult_3_label
  
  if (df$X1.1.1a == permanent_establishment_constants$YES | df$X1.1.1b == permanent_establishment_constants$YES){
    pred_prob <- 1
    pred_label <- permanent_establishment_constants$PERMANENT_ESTABLISHMENT
    
  } else if (df$X1.1.1a == permanent_establishment_constants$NO | df$X1.1.1b == permanent_establishment_constants$NO){
    pred_prob <- 1
    pred_label <- permanent_establishment_constants$NO_PERMANENT_ESTABLISHMENT
    
  } else if (subresult_1_label == permanent_establishment_constants$FPOB_PE | subresult_2_label == permanent_establishment_constants$DA_PE){
    pred_label <- permanent_establishment_constants$PERMANENT_ESTABLISHMENT
    if (subresult_1_label == permanent_establishment_constants$FPOB_PE & subresult_2_label == permanent_establishment_constants$NO_DA_PE){
      pred_prob <- subresult_1_prob
    } else if (subresult_1_label == permanent_establishment_constants$NO_FPOB_PE & subresult_2_label == permanent_establishment_constants$DA_PE){
      pred_prob <- subresult_2_prob
    } else if (subresult_1_label == permanent_establishment_constants$FPOB_PE & subresult_2_label == permanent_establishment_constants$DA_PE){
      pred_prob <- max(convert_probability_above_50(subresult_1_prob), convert_probability_above_50(subresult_2_prob))
    }
    
  } else if ((subresult_1_label == permanent_establishment_constants$NO_FPOB_PE & subresult_2_label == permanent_establishment_constants$NO_DA_PE) & 
             subresult_3_label == permanent_establishment_constants$SERVICES_PE){
    pred_label <- permanent_establishment_constants$PERMANENT_ESTABLISHMENT
    pred_prob <- 1
    
  } else{
    pred_label <- permanent_establishment_constants$NO_PERMANENT_ESTABLISHMENT
    pred_prob <- min(convert_probability_above_50(subresult_1_prob), convert_probability_above_50(subresult_2_prob))
  }
  
  return(list(
    prediction = pred_label,
    probability = pred_prob,
    subResults = list(
      list(
        group = 'FPOB',
        prediction = subresult_1_label,
        probability=subresult_1_prob
      ),
      list(
        group = 'DA',
        prediction = subresult_2_label,
        probability=subresult_2_prob
      )
    )
  ))
}

################################################################################
################################################################################
################################################################################
################################################################################

permanent_establishment_factors_old <- function(df){
  
  labels_fpob <- attributes(permanent_establishment_fpob_model$terms)$term.labels
  labels_fpob <- gsub('^X', '', labels_fpob)
  labels_fpob <- labels_fpob[-which(c('1.4log', '1.5log') %in% labels_fpob)]
  labels_fpob <- c(labels_fpob, c('1.4', '1.5'))
  
  
  labels_da <- attributes(permanent_establishment_da_model$terms)$term.labels
  labels_da <- gsub('^X', '', labels_da)
  
  hardcode_factors <- c('1.1', '2.1', '2.3', '2.11')
  
  factors <- c(labels_fpob, labels_da, hardcode_factors)
  return(factors)
}

permanent_establishment_transform_data_old <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors_old, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X1.4', 'X1.5', 'X1.15'))
  
  if ("X1.4" %in% answered_questions){
    df$X1.4log <- log(df$X1.4)
  }
  
  if ("X1.5" %in% answered_questions){
    df$X1.5log <- log(df$X1.5)
  }
  
  df <- empty_question_transformation(df, permanent_establishment_factors_old, TRUE, answered_questions)
  
  return(df)
  
}

fpob_model_use_old <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors_old, FALSE, answered_questions)
  
  if (df$X1.1 == permanent_establishment_constants$YES){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

da_model_use_old <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors_old, FALSE, answered_questions)
  
  if (df$X2.1 == permanent_establishment_constants$YES &
      df$X2.3 == permanent_establishment_constants$YES & 
      df$X2.11 != permanent_establishment_constants$NO){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

permanent_establishment_da_logic_old <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, permanent_establishment_factors_old, FALSE, answered_questions)
  
  if (df$X2.1 == permanent_establishment_constants$YES &
      df$X2.3 == permanent_establishment_constants$YES &
      df$X2.11 == permanent_establishment_constants$NO){
    da_prob <- 0.95
    da_label <- permanent_establishment_constants$DA_PE
  } else {
    da_prob <- 1
    da_label <- permanent_establishment_constants$NO_DA_PE
  }
  return(list(
    da_prob=da_prob,
    da_label=da_label
  ))
}

permanent_establishment_calculate_prediction <- function(da_prob, da_label, fpob_prob, fpob_label){
  
  if (da_label==permanent_establishment_constants$DA_PE & fpob_label==permanent_establishment_constants$FPOB_PE){
    pred_prob <- max(convert_probability_above_50(da_prob), convert_probability_above_50(fpob_prob))
    pred_label <- permanent_establishment_constants$PERMANENT_ESTABLISHMENT
  } else if (da_label==permanent_establishment_constants$DA_PE) {
    pred_prob <- da_prob
    pred_label <- permanent_establishment_constants$PERMANENT_ESTABLISHMENT
  } else if (fpob_label==permanent_establishment_constants$FPOB_PE){
    pred_prob <- fpob_prob
    pred_label <- permanent_establishment_constants$PERMANENT_ESTABLISHMENT
  } else {
    pred_prob <- min(convert_probability_above_50(da_prob), convert_probability_above_50(fpob_prob))
    pred_label <- permanent_establishment_constants$NO_PERMANENT_ESTABLISHMENT
  }
  
  return(list(
    pred_prob=pred_prob,
    pred_label=pred_label
  ))
}

permanent_establishment_predict_old <- function(df){
  
  df <- permanent_establishment_transform_data_old(df)
  
  if (fpob_model_use_old(df)){
    fpob_prob <- predict(permanent_establishment_fpob_model, df, type = 'response')
    fpob_label <- ifelse(fpob_prob > 0.5, permanent_establishment_constants$NO_FPOB_PE, permanent_establishment_constants$FPOB_PE)
  } else {
    fpob_prob <- 1
    fpob_label <- permanent_establishment_constants$NO_FPOB_PE
  }
  
  if (da_model_use_old(df)){
    da_prob <- predict(permanent_establishment_da_model, df, type = 'response')
    da_label <- ifelse(da_prob > 0.5, permanent_establishment_constants$NO_DA_PE, permanent_establishment_constants$DA_PE)
  } else {
    da_logic <- permanent_establishment_da_logic_old(df)
    da_prob <- da_logic$da_prob
    da_label <- da_logic$da_label
  }
  
  prediction <- permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)
  
  pred_label <- prediction$pred_label
  pred_prob <- prediction$pred_prob
  
  return(list(
    prediction = pred_label,
    probability = pred_prob,
    subResults = list(
      list(
        group = 'FPOB',
        prediction = fpob_label,
        probability=fpob_prob
      ),
      list(
        group = 'DA',
        prediction = da_label,
        probability=da_prob
      )
    )
  ))
}






