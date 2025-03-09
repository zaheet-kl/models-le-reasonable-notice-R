prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

related_persons_and_dealing_at_arms_length_aic_model <- readRDS(prophetPath('models', 'caTaxForesight', 'relatedPersonsAndDealingAtArmsLength', 'aic.rds'))
related_persons_and_dealing_at_arms_length_cm_model <- readRDS(prophetPath('models', 'caTaxForesight', 'relatedPersonsAndDealingAtArmsLength', 'cm.rds'))
related_persons_and_dealing_at_arms_length_dfc_model <- readRDS(prophetPath('models', 'caTaxForesight', 'relatedPersonsAndDealingAtArmsLength', 'dfc.rds'))


YES <- 'Yes'
NOPE <- 'No'

UNRELATED <- 'unrelated'
RELATED <- 'related'
ACTING <- 'acting'
NOT_ACTING <- 'not_acting'
COM <- 'com'
NOT_COM <- 'not_com'
DFC <- 'dfc'
NOT_DFC <- 'not_dfc'
ARM <- 'arm'
NOT_ARM <- 'not_arm'

related_persons_and_dealing_at_arms_length_factors <- function(){
  
  return(c(
    '1.0',
    '1.2',
    '1.2.1',
    '2.1N',
    '2.1NA',
    '2.1NA.1',
    '2.1NB',
    '2.1NC',
    '2.1ND',
    '2.1O1',
    '2.1O2',
    '2.1C',
    '2.2C',
    '2.3C.1',
    '2.3C.2',
    '2.4C',
    '3.1_customChoiceValue',
    '3.2',
    '3.3',
    '3.4', 
    '3.5',
    '3.6',
    '3.7',
    '3.8.1',
    '3.8.2',
    '3.8.3',
    '3.9.1',
    '3.9.2',
    '3.9.3',
    '3.9.4',
    '3.10_customChoiceValue',
    '3.11',
    '3.12',
    '3.13.1',
    '3.13.2',
    '3.13.3',
    '3.13.4',
    '3.0T.1.onehumanA',
    '3.0T.1.onehumanB',
    '3.0T.3.onehuman',
    '3.0T.4.onehuman',
    '3.0T.1.onecorpA',
    '3.0T.3.onecorp',
    '3.0T.4.onecorp',
    '3.0T.1.onetrustA',
    '3.0T.3.onetrust',
    '3.0T.4.onetrust',
    '3.0T.1.bothtrustA',
    '3.0T.2.bothtrustA',
    '3.0T.2.bothtrustB',
    '3.0T.3.bothtrustA',
    '3.0T.3.bothtrustB',
    '3.0P.1.onepartnership',
    '3.0P.1.bothpartnership',
    '3.0P.1.onehuman',
    '3.0P.1.onecorp'
  ))
  
}

related_persons_and_dealing_at_arms_length_transform_data <- function(df){

  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, related_persons_and_dealing_at_arms_length_factors, FALSE, answered_questions)
  
  df$X3.8 <- rowSums(df[,c('X3.8.1', 'X3.8.2', 'X3.8.3')] == TRUE)
  df$X3.9 <- rowSums(df[,c('X3.9.1', 'X3.9.2', 'X3.9.3', 'X3.9.4')] == TRUE)
  df$X3.13 <- rowSums(df[,c('X3.13.1', 'X3.13.2', 'X3.13.3', 'X3.13.4')] == TRUE)
  
  df$X3.2 <- df$X3.2 %>% 
    recode('Not applicable because neither person is capable of owning the other' =
             'No')
  
  df$X3.3 <- df$X3.3 %>% 
    recode('Not applicable because neither person can be the executive-level decision-maker of the other’s business' =
             'No')
  
  df <- format_dataframe_numerics(df, c('X3.8', 'X3.9', 'X3.13'))
  
  df <- empty_question_transformation(df, related_persons_and_dealing_at_arms_length_factors, TRUE, answered_questions)
  
  return(df)
}

related_persons_and_dealing_at_arms_length_logic <- function(df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob) {
  
  answered_questions <- names(df)
  df <- empty_question_transformation(df, related_persons_and_dealing_at_arms_length_factors, FALSE, answered_questions)
  
  if(grepl('child', df$X2.1NA) |
     grepl('dependent', df$X2.1NA) |
     grepl('brother', df$X2.1NA.1) |
     grepl('married', df$X2.1NB) |
     grepl('partner', df$X2.1NC) |
     grepl('adopted', df$X2.1ND) |
     grepl('controls', df$X2.1O1) |
     grepl('controls', df$X2.1O2) |
     grepl('controls', df$X2.1C) |
     grepl('controls', df$X2.2C) |
     grepl('shares', df$X2.3C.1) |
     df$X2.3C.2 == YES |
     df$X2.4C == YES){
    pred_label <- RELATED
    pred_prob <- 1
    gauge_label <- NOT_ARM
  } else if(df$X1.0 == 'Yes, I only want to know if the persons are related' &
            (df$X1.2 == 'The other person is neither an individual natural person nor a corporation' |
             df$X1.2.1 == 'Neither of them' |
             df$X2.1N == 'None of these categories describes the connection between the persons at all' |
             df$X2.1NA.1 == 'None of the above descriptions apply' |
             df$X2.1NB == 'None of the above descriptions apply' |
             df$X2.1NC == 'None of the above descriptions apply' |
             df$X2.1ND == 'None of the above descriptions apply' |
             df$X2.1O1 == 'None of the above descriptions apply' |
             df$X2.1O2 == 'None of the above descriptions apply' |
             df$X2.2C == 'None of the above descriptions apply' |
             df$X2.3C.1 == 'Neither of the above descriptions apply' |
             df$X2.3C.2 == NOPE |
             df$X2.4C == NOPE)){
    pred_label <- UNRELATED
    pred_prob <- 1
    gauge_label <- ARM
  } else if(df$X3.0T.1.onehumanA == YES |
            df$X3.0T.1.onehumanB == 'None of the above are true' |
            df$X3.0T.3.onehuman == YES |
            df$X3.0T.4.onehuman == YES |
            df$X3.0T.1.onecorpA == 'None of the above are true' |
            df$X3.0T.3.onecorp == YES |
            df$X3.0T.4.onecorp == YES |
            df$X3.0T.1.onetrustA == 'None of the above are true' |
            df$X3.0T.3.onetrust == YES |
            df$X3.0T.4.onetrust == YES |
            df$X3.0T.1.bothtrustA == 'None of the above are true' |
            df$X3.0T.2.bothtrustA == YES |
            df$X3.0T.2.bothtrustB == YES |
            df$X3.0T.3.bothtrustA == YES |
            df$X3.0T.3.bothtrustB == YES |
            grepl('partnership', df$X3.0P.1.onepartnership) |
            grepl('partnership', df$X3.0P.1.bothpartnership) |
            grepl('partnership', df$X3.0P.1.onehuman) |
            grepl('partnership', df$X3.0P.1.onecorp)){
    pred_label <- NOT_ARM
    pred_prob <- 1
    gauge_label <- NOT_ARM
  } else if(aic_sub == NOT_ACTING &
            cm_sub == NOT_COM &
            dfc_sub == NOT_DFC){
    pred_label <- ARM
    pred_prob <- min(aic_prob, cm_prob, dfc_prob)
    gauge_label <- ARM
  } else if(aic_sub == ACTING |
            cm_sub == COM |
            dfc_sub == DFC){
    pred_label <- NOT_ARM
    all_probs <- c(aic_prob, cm_prob, dfc_prob)
    positive_preds <- all_probs[all_probs < 0.5]
    pred_prob <- min(positive_preds)
    gauge_label <- NOT_ARM
  }
  
  return(list(
    prediction = pred_label,
    probability = pred_prob,
    probabilityPrediction = gauge_label
  ))
}

related_persons_and_dealing_at_arms_length_predict <- function(df) {
  
  df <- related_persons_and_dealing_at_arms_length_transform_data(df)
  
  aic_prob <- ''
  aic_sub <- ''
  aic_gauge <- ''
  cm_prob <- ''
  cm_sub <- ''
  cm_gauge <- ''
  dfc_prob <- ''
  dfc_sub <- ''
  dfc_gauge <- ''
  
  if(df$X3.4 != 'Unused'){
    aic_prob <- predict(related_persons_and_dealing_at_arms_length_aic_model, df, type='response')
    aic_sub <- ifelse(aic_prob>0.5, NOT_ACTING, ACTING)
    aic_gauge <- ifelse(aic_prob < 0.5, 1 - aic_prob, aic_prob)
  } else{
    aic_prob <- 1
    aic_sub <- '__noMl__'
  }
  
  if(df$X3.2 != 'Unused' & df$X3.5 != 'Unused'){
    cm_prob <- predict(related_persons_and_dealing_at_arms_length_cm_model, df, type='response')
    cm_sub <- ifelse(cm_prob>0.5, NOT_COM, COM)
    cm_gauge <- ifelse(cm_prob < 0.5, 1 - cm_prob, cm_prob)
  } else{
    cm_prob <- 1
    cm_sub <- '__noMl__'
  }
  
  if(df$X3.2 != 'Unused'){
    dfc_prob <- predict(related_persons_and_dealing_at_arms_length_dfc_model, df, type='response')
    dfc_sub <- ifelse(dfc_prob>0.5, NOT_DFC, DFC)
    dfc_gauge <- ifelse(dfc_prob < 0.5, 1 - dfc_prob, dfc_prob)
  } else{
    dfc_prob <- 1
    dfc_sub <- '__noMl__'
  }
  
  hardcodes <- related_persons_and_dealing_at_arms_length_logic(df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)
  
  return(list(
    prediction = hardcodes$prediction,
    probability = hardcodes$probability,
    probabilityPrediction = hardcodes$probabilityPrediction,
    subResults = list(
      list(
        group = 'ActinginConcert',
        prediction = aic_sub,
        probability = aic_prob
      ),
      list(
        group = 'CommonMind',
        prediction = cm_sub,
        probability = cm_prob
      ),
      list(
        group = 'DeFactoControl',
        prediction = dfc_sub,
        probability = dfc_prob
      )
    )
  ))
}

related_persons_and_dealing_at_arms_length_interim <- function(df){

  hardcodes <- related_persons_and_dealing_at_arms_length_logic(df)
  
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
    return(create_interim_predictor(related_persons_and_dealing_at_arms_length_model,
                                    ASSOCIATED,
                                    NOT_ASSOCIATED,
                                    data.frame(),
                                    1)(df))
  }
}