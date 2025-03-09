prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

all_events_income_model <- readRDS(prophetPath('models', 'usTaxForesight', 'allEventsIncome', 'aei_lr_v1.rds'))

NO_FIXED_RIGHT <- 'No fixed right to income'
FIXED_RIGHT <- 'Fixed right to income'

ALL_EVENTS_SATISFIED <- 'All Events Test Satisfied'
ALL_EVENTS_NOT_SATISFIED <- 'All Events Test Not Satisfied'

NO <- 'No'

all_events_income_factors <- function(){
  
  labels <- attributes(all_events_income_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('1.2_2', '1.3')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}

all_events_income_logic <- function(df, fixed_right_prob, fixed_right_label){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, all_events_income_factors, FALSE, answered_questions)
  
  if (df$X1.2_2 == NO | df$X1.3 == NO){
    pred_prob <- 1
    pred_label <- ALL_EVENTS_NOT_SATISFIED
  } else if (fixed_right_label == NO_FIXED_RIGHT){
    pred_prob <- fixed_right_prob
    pred_label <- ALL_EVENTS_NOT_SATISFIED
  } else {
    pred_prob <- fixed_right_prob
    pred_label <- ALL_EVENTS_SATISFIED
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}


all_events_income_predict <- function(df){
  
  fixed_right_prob <- predict(all_events_income_model, newdata=df, type='response')
  fixed_right_label <- ifelse(fixed_right_prob>0.5, NO_FIXED_RIGHT, FIXED_RIGHT)
  
  hardcodes <- all_events_income_logic(df, fixed_right_prob, fixed_right_label)
  
  pred_prob <- hardcodes$probability
  pred_label <- hardcodes$prediction
  
  return(list(
    prediction = pred_label,
    probability = pred_prob,
    subResults = list(
      list(
        group = 'righttoincome',
        prediction = fixed_right_label,
        probability = fixed_right_prob
      )
    )
  ))
  
}

all_events_income_interim <- function(df){
  
  all_events_income_interim <- create_interim_predictor(all_events_income_model,
                                                        FIXED_RIGHT,
                                                        NO_FIXED_RIGHT,
                                                        data.frame(),
                                                        1.5)(df)
  
  fixed_right_prob <- all_events_income_interim$probability
  fixed_right_label <- all_events_income_interim$prediction
  
  hardcodes <- all_events_income_logic(df, fixed_right_prob, fixed_right_label)
  
  pred_prob <- hardcodes$probability
  pred_label <- hardcodes$prediction
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}


