prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

directors_liability_model <- readRDS(prophetPath('models', 'caTaxForesight', 'directorsLiability', 'dl_lr_v1.rds'))

directors_liability_constants <- data.frame(NOT_DULY_DILIGENT = 'Not duly diligent',
                                            DULY_DILIGENT = 'Duly diligent',
                                            stringsAsFactors = FALSE)


directors_liability_factors <- function(){
  
  labels <- attributes(directors_liability_model$terms)$term.labels
  labels <- gsub('X4.2log', 'X4.2', labels)
  labels <- gsub('^X', '', labels)
  
  return(labels)
}

directors_liability_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, directors_liability_factors, FALSE, answered_questions)
  
  continuous_vars <- c('X1.3', 'X4.2')
  df <- format_dataframe_numerics(df, continuous_vars)
  
  if ("X4.2" %in% answered_questions){
    df$X4.2log <- log(df$X4.2)
  }
  
  df <- empty_question_transformation(df, directors_liability_factors, TRUE, answered_questions)
  
  return(df)
}

directors_liability_predict <- function(df){
  
  df <- directors_liability_transform_data(df)
  
  pred_prob <- predict(directors_liability_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5,
                       directors_liability_constants$NOT_DULY_DILIGENT,
                       directors_liability_constants$DULY_DILIGENT)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

directors_liability_interim <- function(df){
  
  df <- directors_liability_transform_data(df)
  
  return(create_interim_predictor(directors_liability_model,
                                  directors_liability_constants$DULY_DILIGENT,
                                  directors_liability_constants$NOT_DULY_DILIGENT,
                                  data.frame(),
                                  2)(df))
}
