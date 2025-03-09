prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

assessment_period_model <- readRDS(prophetPath('models', 'caTaxForesight', 'assessmentPeriod', 'ap.rds'))

NOT_NEGLECT_AP <- 'Not attributable to neglect or carelessness'
NEGLECT_AP <- 'Attributable to neglect or carelessness'

assessment_period_factors <- function(df){
  
  labels <- c(
    '1.1',
    '1.2',
    '1.3',
    '1.4',
    '2.1',
    '2.3',
    '2.5',
    '2.6',
    '2.7',
    '2.8',
    '3.2',
    '3.3',
    '3.4',
    '3.5',
    '3.8',
    '4.1',
    '4.1.1',
    '4.2',
    '4.3',
    '4.4',
    '4.5',
    '5.1',
    '5.2'
  )

  return(labels)
}

assessment_period_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, assessment_period_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X1.3', 'X3.2', 'X3.3'))
  
  if ("X3.2" %in% answered_questions &
      "X3.3" %in% answered_questions){
    df$X3.33 <- log(1 + df$X3.2/(1 + df$X3.3))
  }
  
  df <- empty_question_transformation(df, assessment_period_factors, TRUE, answered_questions)
  
  return(df)
  
}

assessment_period_predict <- function(df){
  
  df <- assessment_period_transform_data(df)
  
  pred_prob <- predict(assessment_period_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5, NOT_NEGLECT_AP, NEGLECT_AP)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

assessment_period_interim <- function(df){
  
  df <- assessment_period_transform_data(df)
  
  return(create_interim_predictor(assessment_period_model,
                                  NEGLECT_AP,
                                  NOT_NEGLECT_AP,
                                  data.frame(),1)(df))
}
