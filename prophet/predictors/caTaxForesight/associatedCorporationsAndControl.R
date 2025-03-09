prophetSource('interim_analysis', 'interim_analysis.R')

associated_corporations_and_control_model <- readRDS(prophetPath('models', 'caTaxForesight', 'associatedCorporationsAndControl', 'acc.rds'))

YES <- 'Yes'
NOPE <- 'No'
ASSOCIATED <- 'Associated corporations'
ASSOCIATED_BUT_125 <- 'Associated corporations but not for section 125'
CONTROLLED <- 'Controlled corporation'
NOT_ASSOCIATED <- 'Not associated corporations'
NOT_CONTROLLED <- 'Not controlled corporation'
NOT_ASSOCIATED_OR_CONTROLLED_GAUGE <- 'Not associated or controlled'
ASSOCIATED_OR_CONTROLLED_GAUGE <- 'Associated or controlled'

associated_corporations_and_control_factors <- function(){
  
  labels <- attributes(associated_corporations_and_control_model$terms)$term.labels
  labels <- gsub('X', '', labels)
  labels <- c(labels,
              '1.1_customChoiceValue',
              '1.1.01',
              '1.1A',
              '1.1A.02',
              '1.1A.1',              
              '1.1A.2',
              '1.2',
              '1.2.1',
              '1.2.2',
              '1.2.01',
              '1.2.02',
              '1.3_customChoiceValue',
              '1.3.01',
              '1.3A',
              '1.3A.02',
              '1.3A.1',             
              '1.3A.2',
              '1.3B',
              '1.3B.02',
              '1.3B.1',
              '1.3B.2',
              '1.3C_customChoiceValue',
              '1.4A',
              '1.4A.01',
              '1.4A.02',
              '1.4B',
              '1.4B.01',
              '1.4B.02',
              '1.4C',
              '1.4C.01',
              '1.4C.02',
              '3.1.1',
              '3.1.2',
              '3.1.3',
              '3.1.4',
              '3.1.5',
              '3.1.6',
              '3.1.7')
  
  return(labels)
}

associated_corporations_and_control_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, associated_corporations_and_control_factors, FALSE, answered_questions)
  
  continuous_vars <- c('X2.4', 'X2.6', 'X2.7')
  factor_vars <- c('X2.1_customChoiceValue', 'X2.2_customChoiceValue', 'X2.3', 'X2.5', 'X2.8_customChoiceValue', 'X2.9_customChoiceValue')
  hardcode_vars <- c('X1.1_customChoiceValue',
                     'X1.1.01',
                     'X1.1A',
                     'X1.1A.02',
                     'X1.1A.1',              
                     'X1.1A.2',
                     'X1.2',
                     'X1.2.1',
                     'X1.2.2',
                     'X1.2.01',
                     'X1.2.02',
                     'X1.3_customChoiceValue',
                     'X1.3.01',
                     'X1.3A',
                     'X1.3A.02',
                     'X1.3A.1',             
                     'X1.3A.2',
                     'X1.3B',
                     'X1.3B.02',
                     'X1.3B.1',
                     'X1.3B.2',
                     'X1.3C_customChoiceValue',
                     'X1.4A',
                     'X1.4A.01',
                     'X1.4A.02',
                     'X1.4B',
                     'X1.4B.01',
                     'X1.4B.02',
                     'X1.4C',
                     'X1.4C.01',
                     'X1.4C.02',
                     'X3.1.1',
                     'X3.1.2',
                     'X3.1.3',
                     'X3.1.4',
                     'X3.1.5',
                     'X3.1.6',
                     'X3.1.7')
  
  factor_inds <- which(!(names(df) %in% c(continuous_vars, hardcode_vars)))
  continuous_inds <- which(!(names(df) %in% c(factor_vars, hardcode_vars)))
  
  df <- df %>% 
    mutate(X3.1 = grepl(TRUE, paste(df$X3.1.1, df$X3.1.2, df$X3.1.3, df$X3.1.4, df$X3.1.5, df$X3.1.6, df$X3.1.7)))
  
  df$`X1.4A.01` <- df$`X1.4A.01` %>% 
    recode('None of the above - the related persons did not own any shares of either corporation other than term preferred shares or shares of a specified class' =
             'none')
  
  df$`X1.4B.01` <- df$`X1.4B.01` %>% 
    recode('None of the above - the person did not own any shares of the other corporation' =
             'none')
  
  df$`X1.4C.01` <- df$`X1.4C.01` %>% 
    recode('None of the above - no members of the related groups owned any shares of the corporations' =
             'none')
  
  
  df[ , factor_inds] <- lapply(df[ , factor_inds], as.factor)
  df[ , continuous_inds] <- lapply(df[ , continuous_inds], as.numeric)

  df <- empty_question_transformation(df, associated_corporations_and_control_factors, TRUE, answered_questions)
  
  return(df)
}

associated_corporations_and_control_logic <- function(df, ml_prob, classification) {
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, associated_corporations_and_control_factors, FALSE, answered_questions)
  
  if(df$`X3.1` == FALSE &
     (df$`X1.1A` == YES |
      df$`X1.1A.02` == YES |
      df$`X1.1A.1` == YES |
      df$`X1.1A.2` == YES)){
    pred_label <- CONTROLLED
    pred_prob <- 1
  } else if(df$`X3.1` == TRUE &
            (df$`X1.1A` == YES |
             df$`X1.1A.02` == YES |
             df$`X1.1A.1` == YES |
             df$`X1.1A.2` == YES)){
    pred_label <- NOT_CONTROLLED
    pred_prob <- 1
  } else if(df$`X1.2` == YES){
    pred_label <- NOT_ASSOCIATED
    pred_prob <- 1
  } else if(df$X1.2.01 == YES){
    pred_label <- ASSOCIATED
    pred_prob <- 1
  } else if(df$`X1.2.1` == NOPE |
            df$`X1.2.2` == YES){
    pred_label <- ASSOCIATED_BUT_125
    pred_prob <- 1
  } else if(df$`X1.2.2` == NOPE){
    pred_label <- ASSOCIATED
    pred_prob <- 1
  } else if(df$`X1.3_customChoiceValue` == 'does_not_resemble'){
    pred_label <- NOT_ASSOCIATED
    pred_prob <- 1
  } else if(df$`X3.1` == FALSE &
            (df$`X1.3A` == YES |
             df$`X1.3A.02` == YES |
             df$`X1.3A.1` == YES |
             df$`X1.3A.2` == YES |
             df$`X1.3B` == YES |
             df$`X1.3B.02` == YES |
             df$`X1.3B.1` == YES |
             df$`X1.3B.2` == YES)){
    pred_label <- ASSOCIATED
    pred_prob <- 1
  } else if(df$`X3.1` == TRUE &
            (df$`X1.3A` == YES |
             df$`X1.3A.02` == YES |
             df$`X1.3A.1` == YES |
             df$`X1.3A.2` == YES |
             df$`X1.3B` == YES |
             df$`X1.3B.02` == YES |
             df$`X1.3B.1` == YES |
             df$`X1.3B.2` == YES)){
    pred_label <- NOT_ASSOCIATED
    pred_prob <- 1
  } else if(df$`X1.3C_customChoiceValue` == 'none'){
    pred_label <- NOT_ASSOCIATED
    pred_prob <- 1
  } else if(df$`X1.4A` == YES |
            df$`X1.4A.02` == YES |
            df$`X1.4B` == YES |
            df$`X1.4B.02` == YES |
            df$`X1.4C` == YES |
            df$`X1.4C.02` == YES){
    pred_label <- ASSOCIATED
    pred_prob <- 1
  } else if(df$`X1.4A` == NOPE |
            df$`X1.4A.01` == 'none' |
            df$`X1.4A.02` == NOPE |
            df$`X1.4B` == NOPE |
            df$`X1.4B.01` == 'none' |
            df$`X1.4B.02` == NOPE |
            df$`X1.4C` == NOPE |
            df$`X1.4C.01` == 'none' |
            df$`X1.4C.02` == NOPE){
    pred_label <- NOT_ASSOCIATED
    pred_prob <- 1
  } else if(df$`X1.1_customChoiceValue` == 'controls' &
            df$`X3.1` == FALSE &
            classification == ASSOCIATED){
    pred_label <- CONTROLLED
    pred_prob <- ml_prob
  } else if(df$`X1.1_customChoiceValue` == 'controls' &
            df$`X3.1` == TRUE){
    pred_label <- NOT_CONTROLLED
    pred_prob <- 1
  } else if(df$`X1.1_customChoiceValue` == 'controls' &
            classification == NOT_ASSOCIATED){
    pred_label <- NOT_CONTROLLED
    pred_prob <- ml_prob
  } else if(df$`X1.1_customChoiceValue` == 'associated' &
            df$`X3.1` == FALSE &
            classification == ASSOCIATED){
    pred_label <- ASSOCIATED
    pred_prob <- ml_prob
  } else if(df$`X1.1_customChoiceValue` == 'associated' &
            df$`X3.1` == TRUE){
    pred_label <- NOT_ASSOCIATED
    pred_prob <- 1
  } else {
    pred_label <- classification
    pred_prob <- ml_prob
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

associated_corporations_and_control_predict <- function(df) {
  
  df <- associated_corporations_and_control_transform_data(df)
  
  if(df$`X2.5` != 'Unused'){
    ml_prob <- predict(associated_corporations_and_control_model, df, type='response')
    classification <- ifelse(ml_prob>0.5, NOT_ASSOCIATED, ASSOCIATED)
  } else{
    ml_prob <- 1
    classification <- '__noMl__'
  }
  
  hardcodes <- associated_corporations_and_control_logic(df, ml_prob, classification)
  
  if(hardcodes$prediction == ASSOCIATED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- ASSOCIATED_OR_CONTROLLED_GAUGE
  } else if(hardcodes$prediction == NOT_ASSOCIATED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- NOT_ASSOCIATED_OR_CONTROLLED_GAUGE
  } else if(hardcodes$prediction == CONTROLLED |
            hardcodes$prediction == ASSOCIATED_BUT_125){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- ASSOCIATED_OR_CONTROLLED_GAUGE
  } else {
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- NOT_ASSOCIATED_OR_CONTROLLED_GAUGE
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob,
    probabilityPrediction=gauge_label
  ))
}

associated_corporations_and_control_interim <- function(df){
  
  df <- associated_corporations_and_control_transform_data(df)
  
  acc_interim <- create_interim_predictor(associated_corporations_and_control_model,
                                          ASSOCIATED,
                                          NOT_ASSOCIATED,
                                          data.frame(),
                                          1)(df)
  
  ml_prob <- acc_interim$probability
  ml_prob <- acc_interim$prediction

  hardcodes <- associated_corporations_and_control_logic(df, ml_prob, classification)
  
  if(hardcodes$prediction == ASSOCIATED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- ASSOCIATED_OR_CONTROLLED_GAUGE
    
    return(list(
      prediction=pred_prob,
      probability=pred_prob,
      probabilityPrediction=gauge_label
    ))
  } else if(hardcodes$prediction == NOT_ASSOCIATED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- NOT_ASSOCIATED_OR_CONTROLLED_GAUGE
    
    return(list(
      prediction=pred_label,
      probability=pred_prob,
      probabilityPrediction=gauge_label
    ))
  } else if(hardcodes$prediction == CONTROLLED |
            hardcodes$prediction == ASSOCIATED_BUT_125){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- ASSOCIATED_OR_CONTROLLED_GAUGE
    
    return(list(
      prediction=pred_label,
      probability=pred_prob,
      probabilityPrediction=gauge_label
    ))
  } else if(hardcodes$prediction == NOT_CONTROLLED){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    gauge_label <- NOT_ASSOCIATED_OR_CONTROLLED_GAUGE
    
    return(list(
      prediction=pred_label,
      probability=pred_prob,
      probabilityPrediction=gauge_label
    ))
  } else {
    return(create_interim_predictor(associated_corporations_and_control_model,
                                    ASSOCIATED,
                                    NOT_ASSOCIATED,
                                    data.frame(),
                                    1)(df))
  }
}