prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

central_management_and_control_who_model <- readRDS(prophetPath('models', 'caTaxForesight', 'centralManagementAndControl', 'cmc_who_lr_v1.rds'))
central_management_and_control_where_model <- readRDS(prophetPath('models', 'caTaxForesight', 'centralManagementAndControl', 'cmc_where_lr_v1.rds'))

central_management_and_control_constants <- data.frame(OTHER_DECISION_MAKER='Other decision maker',
                                                       BOARD_OF_DIRECTORS='Board of directors',
                                                       CMC_NOT_CANADA='Central management and control is not in Canada',
                                                       CMC_CANADA='Central management and control is in Canada',
                                                       NOT_RELEVANT='Not relevant',
                                                       stringsAsFactors=FALSE)

central_management_and_control_factors <- function(){
  return(c("1.1", "1.4", "1.5",
           "2.1.1", "2.2.1", "2.3.1", "2.4.1", "2.5.1", "2.6.1", "2.7.1", "2.8.1", "2.9.1",
           "2.10.1", "2.11.1", "2.12.1", "2.1.2", "2.2.2", "2.3.2", "2.4.2", "2.5.2", "2.6.2",
           "2.7.2", "2.8.2", "2.9.2", "2.10.2", "2.11.2", "2.12.2",
           "3.1", "3.2", "3.3",
           "4.2", "4.3", "4.4", "4.5",
           "5.1", "5.1.1", "5.2", "5.3", "5.4"))
}

central_management_and_control_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, central_management_and_control_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X1.5'))
  
  if (df$X4.5 == 'Not Applicable'){
    df$X4.5 <- 'Not relevant'
  }
  
  if (df$X5.1 == 'Not Applicable'){
    df$X5.1 <- 'Not relevant'
  }
  
  df <- empty_question_transformation(df, central_management_and_control_factors, TRUE, answered_questions)
  
  return(df)
}

central_management_and_control_predict_who <- function(df){
  
  pred_prob <- predict(central_management_and_control_who_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5,
                       central_management_and_control_constants$OTHER_DECISION_MAKER,
                       central_management_and_control_constants$BOARD_OF_DIRECTORS)
  
  return(list(
    who_prob=pred_prob,
    who_label=pred_label))
}

central_management_and_control_where_data <- function(df, who_label){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, central_management_and_control_factors, FALSE, answered_questions)
  
  if (who_label == central_management_and_control_constants$OTHER_DECISION_MAKER){
    df$Xlives <- df$X5.3
    df$Xdecisions <- df$X5.4
  } else if (who_label == central_management_and_control_constants$BOARD_OF_DIRECTORS){
    df$Xlives <- df$X5.2
    if (df$X5.1 == central_management_and_control_constants$NOT_RELEVANT){
      df$Xdecisions <- df$X5.1.1
    } else {
      df$Xdecisions <- df$X5.1
    }
  }
  
  df <- empty_question_transformation(df, central_management_and_control_factors, TRUE, answered_questions)
  
  return(df)
}

central_management_and_control_predict <- function(df){
  
  df <- central_management_and_control_transform_data(df)
  
  who <- central_management_and_control_predict_who(df)
  
  who_prob <- who$who_prob
  who_label <- who$who_label
  
  df <- central_management_and_control_where_data(df, who_label)
  
  where_prob <- predict(central_management_and_control_where_model, newdata=df, type='response')
  pred_label <- ifelse(where_prob>0.5,
                       central_management_and_control_constants$CMC_NOT_CANADA,
                       central_management_and_control_constants$CMC_CANADA)
  
  pred_prob <- (convert_probability_above_50(where_prob) + convert_probability_above_50(who_prob)) / 2

  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

central_management_and_control_interim <- function(df){
  
  df <- central_management_and_control_transform_data(df)
  
  cmc_who_interim <- create_interim_predictor(central_management_and_control_who_model,
                                              central_management_and_control_constants$BOARD_OF_DIRECTORS,
                                              central_management_and_control_constants$OTHER_DECISION_MAKER,
                                              data.frame(),
                                              1)(df)
  
  who_prob <- cmc_who_interim$probability
  who_label <- cmc_who_interim$prediction
  
  df <- central_management_and_control_where_data(df, who_label)
  
  cmc_where_interim <- create_interim_predictor(central_management_and_control_where_model,
                                                central_management_and_control_constants$CMC_CANADA,
                                                central_management_and_control_constants$CMC_NOT_CANADA,
                                                data.frame(),
                                                1)(df)
  
  where_prob <- cmc_where_interim$probability
  pred_label <- cmc_where_interim$prediction
  
  pred_prob <- (convert_probability_above_50(where_prob) + convert_probability_above_50(who_prob)) / 2
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}




