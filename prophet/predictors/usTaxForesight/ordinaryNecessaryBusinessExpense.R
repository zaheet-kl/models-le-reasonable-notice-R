prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

ordinary_necessary_main_model <- readRDS(prophetPath('models', 'usTaxForesight', 'ordinaryNecessaryBusinessExpense', 'donbe_main.rds'))
ordinary_necessary_sub_model <- readRDS(prophetPath('models', 'usTaxForesight', 'ordinaryNecessaryBusinessExpense', 'donbe_sub.rds'))

DONBE <- 'Ordinary and Necessary Business Expense'
NO_DONBE <- 'Not an Ordinary and Necessary Business Expense'

CAPITAL_DONBE <- 'Capital'
NOT_CAPITAL_DONBE <- 'Not capital'

YES <- 'Yes'
NO <- 'No'
NOT_ON <- 'Not ON'
NOT_APPLICABLE = 'Not applicable'

ordinary_necessary_factors <- function(){
  
  labels_main <- attributes(ordinary_necessary_main_model$terms)$term.labels
  labels_main <- gsub('^X', '', labels_main)
  labels_sub <- attributes(ordinary_necessary_sub_model$terms)$term.labels
  labels_sub <- gsub('^X', '', labels_sub)
  
  hardcode_factors <- c('1.3', '1.3.1', '4.5')
  
  labels <- c(labels_main, labels_sub, hardcode_factors)
  
  return(labels)
}

ordinary_necessary_logic <- function(df, sub_prob, sub_label, main_prob, main_label){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, ordinary_necessary_factors, FALSE, answered_questions)
  
  if ((df$X1.3 == YES & df$X1.3.1 == NO) |
      (df$X3.1_customChoiceValue == NOT_ON & df$X3.2 == NO & df$X3.3 == NO & df$X3.4 == NO & df$X3.5 == NO) |
      (df$X4.1 == NO & df$X4.2 == NO & df$X4.3 == NO & df$X4.4 == NO & df$X4.5 == NO & df$X4.6 == NOT_APPLICABLE)){
    pred_prob <- 1
    pred_label <- NO_DONBE
  } else if (sub_label == CAPITAL_DONBE){
    pred_prob <- sub_prob
    pred_label <- NO_DONBE
  } else {
    pred_prob <- (convert_probability_above_50(sub_prob) + convert_probability_above_50(main_prob))/2
    pred_label <- main_label
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

ordinary_necessary_predict <- function(df){
  
  main_prob <- predict(ordinary_necessary_main_model, df, type='response')
  main_label <- ifelse(main_prob>0.5, DONBE, NO_DONBE)
  
  sub_prob <- predict(ordinary_necessary_sub_model, df, type='response')
  sub_label <- ifelse(sub_prob>0.5, NOT_CAPITAL_DONBE, CAPITAL_DONBE)
  
  hardcodes <- ordinary_necessary_logic(df, sub_prob, sub_label, main_prob, main_label)
  
  pred_prob <- hardcodes$probability
  pred_label <- hardcodes$prediction
  
  return(list(
    prediction = pred_label,
    probability = pred_prob,
    subResults = list(
      list(
        group = 'Capital',
        prediction = sub_label,
        probability=sub_prob
      )
    )
  ))
}

ordinary_necessary_interim <- function(df){
  
  main_interim <- create_interim_predictor(ordinary_necessary_main_model,
                                           NO_DONBE,
                                           DONBE,
                                           data.frame(),
                                           1)(df)
  main_prob <- main_interim$probability
  main_label <- main_interim$prediction
  
  sub_interim <- create_interim_predictor(ordinary_necessary_sub_model,
                                          CAPITAL_DONBE,
                                          NOT_CAPITAL_DONBE,
                                          data.frame(),
                                          1)(df)
  sub_prob <- sub_interim$probability
  sub_label <- sub_interim$prediction
  
  hardcodes <- ordinary_necessary_logic(df, sub_prob, sub_label, main_prob, main_label)
  
  pred_prob <- hardcodes$probability
  pred_label <- hardcodes$prediction
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}



