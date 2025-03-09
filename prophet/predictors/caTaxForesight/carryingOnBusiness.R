prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

carrying_on_business_model <- readRDS(prophetPath('models', 'caTaxForesight', 'carryingOnBusiness', 'cob_lr_v1.rds'))

CARRYING_ON_BUSINESS <- 'Carrying on business in Canada'
NOT_CARRYING_ON_BUSINESS <- 'Not carrying on business in Canada'
NO_HARDCODE <- "__noHardcode__"

YES <- 'Yes'
NO <- 'No'

ITA_COB <- 'Income Tax Act'
ETA_COB <- 'Excise Tax Act'

cob_checkbox_questions <- c('3.1.1', '3.1.2', '3.1.3', '3.1.4', '3.1.5', '3.1.6', '3.1.7',
                            '3.1.8', '3.1.9', '3.1.10')

carrying_on_business_factors <- function(df){
  
  labels <- attributes(carrying_on_business_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('0.1', '1.3', '1.4', '2.1', '2.2', '2.3', '2.4',
                        '3.2', '3.3', '3.4', '3.5', '3.6', '3.7', '3.8')
  
  labels <- c(labels, hardcode_factors, cob_checkbox_questions)
  
  return(labels)
  
}

carrying_on_business_control <- function(df){ 
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, carrying_on_business_factors, FALSE, answered_questions)
  
  if (df$X2.1 == YES | df$X2.2 == YES | df$X2.3 == YES | df$X2.4 == YES){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

carrying_on_business_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  
  df <- empty_question_transformation(df, carrying_on_business_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X4.2'))
  
  df <- empty_question_transformation(df, carrying_on_business_factors, TRUE, answered_questions)
  
  return(df)
}

carrying_on_business_logic <- function(df, control){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, carrying_on_business_factors, FALSE, answered_questions)
  
  if (df$X1.3 == NO & df$X1.4 == NO & df$X2.1 == NO & df$X2.2 == NO & df$X2.3 == NO & df$X2.4 == NO){
    pred_prob <- 1
    pred_label <- NOT_CARRYING_ON_BUSINESS
  } else if (df$X0.1 == ITA_COB &
             (df$X1.3 == YES | df$X1.4 == YES | control) &
             (any(df[,paste('X', cob_checkbox_questions, sep='')] == TRUE) |
              df$X3.2 == YES | df$X3.3 == YES | df$X3.4 == YES |
              (df$X3.5 == YES & df$X3.6 == NO))){
    pred_prob <- 1
    pred_label <- CARRYING_ON_BUSINESS
  } else if (df$X0.1 == ETA_COB &
             (df$X1.3 == YES | df$X1.4 == YES | control) &
             df$X3.7 == YES &
             df$X3.8 == NO){
    pred_prob <- 1
    pred_label <- CARRYING_ON_BUSINESS
  } else {
    pred_prob <- 1
    pred_label <- NO_HARDCODE
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

carrying_on_business_predict <- function(df){
  
  df <- carrying_on_business_transform_data(df)
  
  control <- carrying_on_business_control(df)
  
  hardcodes <- carrying_on_business_logic(df, control)
  
  if (hardcodes$prediction != NO_HARDCODE){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(carrying_on_business_model, newdata=df, type='response')
    pred_label <- ifelse(pred_prob>0.5, NOT_CARRYING_ON_BUSINESS, CARRYING_ON_BUSINESS)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

carrying_on_business_interim <- function(df){
  
  df <- carrying_on_business_transform_data(df)
  
  control <- carrying_on_business_control(df)
  
  hardcodes <- carrying_on_business_logic(df, control)
  
  if (hardcodes$prediction != NO_HARDCODE){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(carrying_on_business_model,
                                    CARRYING_ON_BUSINESS,
                                    NOT_CARRYING_ON_BUSINESS,
                                    data.frame(),
                                    1)(df))
  }
  
}
