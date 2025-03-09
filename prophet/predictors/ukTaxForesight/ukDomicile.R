prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

uk_domicile_model <- readRDS(prophetPath('models', 'ukTaxForesight', 'ukDomicile','uk_domicile_v15.rds'))

YES <- 'Yes'
NO <- 'No'

NOT_UK_DEEMED <- 'Not Domiciled in UK (Deemed UK Domiciled)'
DEEMED_UK <- 'UK Domiciled (Deemed UK Domiciled)'

NON_DOM_UK <- 'Not Domiciled in UK'
UK_DOM <- 'Domiciled in UK'

uk_domicile_factors <- function(){
  
  labels <- attributes(uk_domicile_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('1.1', '1.2', '2.3new', '4.1', '4.1_0', '4.1_1', '4.1_2','4.1_5', '5.1', '5.2')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}


uk_domicile_transform_data<- function(df){
  
  answered_questions <- names(df)
  df <- empty_question_transformation(df, uk_domicile_factors, FALSE, answered_questions)

}

uk_domicile_logic <- function(df){
  
  if (((df$X2.3new == YES & df$X5.1 == YES & df$X5.2>=15)|df$X5.2>=17|(df$X1.1 == YES & df$X1.2 == 'In the UK' & df$X2.3new == YES & df$X5.1 == YES)) & ((df$X4.1 == NO & df$X4.1_1 == YES)|(df$X4.1_0 ==YES & df$X4.1_5 == 'Somewhere else')|(df$X4.1_2 == YES))){
    pred_label <- NOT_UK_DEEMED
    pred_prob <- 0.6
    pred_confidence <-NON_DOM_UK
  } else if ((df$X4.1 == NO & df$X4.1_1 == YES)|(df$X4.1_0 ==YES & df$X4.1_5 == 'Somewhere else')|(df$X4.1_2 == YES)){
    pred_label <-NON_DOM_UK
    pred_prob <-0.6
    pred_confidence <-NON_DOM_UK
  } else if (((df$X2.3new == YES & df$X5.1 == YES & df$X5.2>=15)|df$X5.2>=17|(df$X1.1 == YES & df$X1.2 == 'In the UK' & df$X2.3new == YES & df$X5.1 == YES)) & ((df$X4.1 == NO & df$X4.1_1 == NO)|(df$X4.1_0 ==YES & df$X4.1_5 == 'In the UK'))){
    pred_label <- DEEMED_UK
    pred_prob <- 0.4
    pred_confidence <-UK_DOM
  } else if ((df$X4.1 == NO & df$X4.1_1 == NO)|(df$X4.1_0 ==YES & df$X4.1_5 == 'In the UK')){
    pred_label <- UK_DOM
    pred_prob <- 0.4
    pred_confidence <-UK_DOM
  } else {
    pred_label <- '__noHardcode__'
    pred_prob <- NA
    pred_confidence <- '__noHardcode__'
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob,
    probabilityPrediction = pred_confidence
  ))
  
}


uk_domicile_predict <- function(df){
  
  df<- uk_domicile_transform_data(df)
  
  hardcodes <- uk_domicile_logic(df)
  
  if (hardcodes$prediction == NOT_UK_DEEMED){
    pred_prob <-hardcodes$probability
    pred_label <- hardcodes$prediction
    pred_confidence <- hardcodes$probabilityPrediction
  } else if (hardcodes$prediction == NON_DOM_UK){
    pred_prob <-hardcodes$probability
    pred_label <- hardcodes$prediction
    pred_confidence <- hardcodes$probabilityPrediction
  } else if (hardcodes$prediction == DEEMED_UK){
    pred_prob <-hardcodes$probability
    pred_label <- hardcodes$prediction
    pred_confidence <- hardcodes$probabilityPrediction
  } else if (hardcodes$prediction == UK_DOM){
    pred_prob <-hardcodes$probability
    pred_label <- hardcodes$prediction
    pred_confidence <- hardcodes$probabilityPrediction
  } else{ 
    if ((df$X2.3new == YES & df$X5.1 == YES & df$X5.2>=15)|df$X5.2>=17|(df$X1.1 == YES & df$X1.2 == 'In the UK' & df$X2.3new == YES & df$X5.1 == YES)){
        pred_prob <- predict(uk_domicile_model, df, type='response')
        pred_label <- ifelse(pred_prob>0.5, NOT_UK_DEEMED, DEEMED_UK)
        pred_confidence <-ifelse(pred_prob>0.5, NON_DOM_UK, UK_DOM)
  
    } else {
      pred_prob <- predict(uk_domicile_model, df, type='response')
      pred_label <- ifelse(pred_prob>0.5, NON_DOM_UK, UK_DOM)
      pred_confidence <-ifelse(pred_prob>0.5, NON_DOM_UK, UK_DOM)
  }
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob,
    probabilityPrediction = pred_confidence
  ))
}


uk_domicile_interim <- function(df){
  
  df<- uk_domicile_transform_data(df)
  
  return(create_interim_predictor(uk_domicile_model,
                                  UK_DOM,
                                  NON_DOM_UK,
                                  data.frame(),
                                  2)(df))
}

