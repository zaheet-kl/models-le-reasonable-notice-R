prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

uk_treaty_residence_model <- readRDS(prophetPath('models', 'ukTaxForesight', 'ukTreatyResidence','uk_treaty_residence.rds'))

uk_treaty_residence_constants <- data.frame(
  FALS = 'FALSE',
  TRU = 'TRUE',
  UNDETERMINED = 'Undetermined',
  UK = 'UK Centre of Vital Interests',
  OTHER = 'Other Centre of Vital Interests',
  NOTAPP = 'Not applicable',
  NO = 'No',
  YES = 'Yes',
  HA = 'Residence Determined by Habitual Abode',
  RESUK = 'Resident in UK',
  RESOTHER = 'Resident in Other Country',
  stringsAsFactors = FALSE
  )

uk_treaty_residence_factors <- function(){
  
  labels <- c(attributes(uk_treaty_residence_model$terms)$term.labels)
  labels <- gsub('^X', '', labels)
  
  hardcode_factors <- c('1.1', '1.3', '2.1.1', '2.1.2')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
  
}

uk_treaty_residence_transform_data <- function(df){
  
  answered_questions <- names(df)
  df <- empty_question_transformation(df, uk_treaty_residence_factors, FALSE, answered_questions)
  
}

uk_treaty_residence_logic <- function(df, cv_prob, cv_label){
  
  # CV subresult
  
  if(df$X2.1.1 == uk_treaty_residence_constants$FALS &
     df$X2.1.2 == uk_treaty_residence_constants$FALS){
    CV <- uk_treaty_residence_constants$NOTAPP
  }
  else if(df$X2.1.1 == uk_treaty_residence_constants$TRU &
          df$X2.1.2 == uk_treaty_residence_constants$TRU &
          cv_label == uk_treaty_residence_constants$UK){
    CV <- uk_treaty_residence_constants$UK
  }
  else if(df$X2.1.1 == uk_treaty_residence_constants$TRU &
          df$X2.1.2 == uk_treaty_residence_constants$TRU &
          cv_label == uk_treaty_residence_constants$OTHER){
    CV <- uk_treaty_residence_constants$OTHER
  }
  else{
    CV <- uk_treaty_residence_constants$NOTAPP
  }
  
  # Overall logic
  if(df$X1.1 == uk_treaty_residence_constants$NO |
     (df$X2.1.2 == uk_treaty_residence_constants$TRU &
     df$X2.1.1 != uk_treaty_residence_constants$TRU)){
    pred_label <- uk_treaty_residence_constants$RESOTHER
    pred_prob <- 1
  }
  else if(df$X1.3 == uk_treaty_residence_constants$NO |
          (df$X2.1.1 == uk_treaty_residence_constants$TRU &
           df$X2.1.2 != uk_treaty_residence_constants$TRU)){
    pred_label <- uk_treaty_residence_constants$RESUK
    pred_prob <- 1
  }
  else if(CV == uk_treaty_residence_constants$UK){
    pred_label <- uk_treaty_residence_constants$RESUK
    pred_prob <- cv_prob
  }
  else if(CV == uk_treaty_residence_constants$OTHER){
    pred_label <- uk_treaty_residence_constants$RESOTHER
    pred_prob <- cv_prob
  }
  else{
    pred_label <- uk_treaty_residence_constants$HA
    pred_prob <- 0.5
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob,
    cv_subresult = CV
  ))
}

uk_treaty_residence_predict <- function(df){
  
  df <- uk_treaty_residence_transform_data(df)
  
  cv_prob_ml <- predict(uk_treaty_residence_model, df, type = 'response')
  cv_label_ml <- ifelse(cv_prob_ml > 0.5, uk_treaty_residence_constants$UK, uk_treaty_residence_constants$OTHER)
  
  hardcodes <- uk_treaty_residence_logic(df,
                                         cv_prob = cv_prob_ml,
                                         cv_label = cv_label_ml)
  
  return(list(
    prediction = hardcodes$prediction,
    probability = hardcodes$probability,
    
    subResults = list(
      list(
        group = 'COVI',
        prediction = hardcodes$cv_subresult,
        probability = 1
      )
    )
  ))
  
}

uk_treaty_residence_interim <- function(df){
  
  df <- uk_treaty_residence_transform_data(df)
  
  uk_treaty_residence_interim_predictions <- create_interim_predictor(uk_treaty_residence_model,
                                                                      uk_treaty_residence_constants$OTHER,
                                                                      uk_treaty_residence_constants$UK,
                                                                      data.frame(),
                                                                      1)(df)
  
  hardcodes <- uk_treaty_residence_logic(df,
                                         cv_prob = uk_treaty_residence_interim_predictions$probability,
                                         cv_label = uk_treaty_residence_interim_predictions$prediction)
  
  return(list(
    prediction=hardcodes$prediction,
    probability=hardcodes$probability
  ))
}