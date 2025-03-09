prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

all_events_expense_model <- readRDS(prophetPath('models', 'usTaxForesight', 'allEventsExpense', 'ae_lr_v3.rds'))

ALL_EVENTS_SATISFIED <- 'All Events Test Satisfied'
ALL_EVENTS_NOT_SATISFIED <- 'All Events Test Not Satisfied'

LIABILITY_SATISIFIED <- 'Liability branch satisfied'
LIABILITY_NOT_SATISFIED <- 'Liability branch not satisfied'

NO <- 'No'
YES <- 'Yes'

all_events_expense_factors <- function(){
  
  labels <- attributes(all_events_expense_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('1.2_customChoiceValue', '2.2', '6.5')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}

all_events_expense_logic <- function(df, liability_prob, liability_pred){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, all_events_expense_factors, FALSE, answered_questions)
  
  if (df$X6.5 == NO |
      df$X1.2_customChoiceValue == NO |
      df$X2.2 == NO){
    pred_prob <- 1
    pred_label <- ALL_EVENTS_NOT_SATISFIED
  } else if (df$X6.5 == YES &
             df$X1.2_customChoiceValue == YES &
             df$X2.2 == YES &
             liability_pred == LIABILITY_SATISIFIED){
    pred_prob <- liability_prob
    pred_label <- ALL_EVENTS_SATISFIED
  } else {
    pred_prob <- liability_prob
    pred_label <- ALL_EVENTS_NOT_SATISFIED
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

all_events_expense_predict <- function(df){

  liability_prob <- predict(all_events_expense_model, newdata=df, type='response')
  liability_label <- ifelse(liability_prob>0.5, LIABILITY_SATISIFIED, LIABILITY_NOT_SATISFIED)
  
  hardcodes <- all_events_expense_logic(df, liability_prob, liability_label)
  
  pred_prob <- hardcodes$probability
  pred_label <- hardcodes$prediction
  
  return(list(
    prediction = pred_label,
    probability = pred_prob,
    subResults = list(
      list(
        group = 'liabilitybranch',
        prediction = liability_label,
        probability = liability_prob
      )
    )
  ))
  
}

all_events_expense_interim <- function(df){
  
  all_events_expense_interim <- create_interim_predictor(all_events_expense_model,
                                                               LIABILITY_NOT_SATISFIED,
                                                               LIABILITY_SATISIFIED,
                                                               data.frame(),
                                                               1.5)(df)
  
  liability_prob <- all_events_expense_interim$probability
  liability_label <- all_events_expense_interim$prediction
  
  hardcodes <- all_events_expense_logic(df, liability_prob, liability_label)
  
  pred_prob <- hardcodes$probability
  pred_label <- hardcodes$prediction
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

