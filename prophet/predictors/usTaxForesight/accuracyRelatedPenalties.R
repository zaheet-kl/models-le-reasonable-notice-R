prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

accuracy_related_penalties_model <- readRDS(prophetPath('models', 'usTaxForesight', 'accuracyRelatedPenalties', 'arp_lr_v5.rds'))

RCGF <- 'RCGF'
NO_RCGF <- 'No RCGF'

LACKING_ECONOMIC_SUBSTANCE <- 'Disallowance of claimed tax benefits by reason of a transaction lacking economic substance (within the meaning of § 7701(o)) or failing to meet the requirements of any similar rule of law (§ 6662(b)(6))'
CHARITABLE_DEDUCTION_PROPERTY <- 'The gross valuation misstatement is an overstatement under chapter 1 with respect to charitable deduction property'


accuracy_related_penalties_factors <- function(){
  
  labels <- attributes(accuracy_related_penalties_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  labels <- c(labels, '1.1', '1.3', '1.4', '1.5', '1.6', '1.7', '1.8')
  
  return(labels)
}

accuracy_related_penalties_transform_data <- function(df){
  
  continuous_vars <- c('X2.1.01', 'X2.5', 'X4.4')
  factor_vars <- c('X1.0.1_customChoiceValue', 'X2.1', 'X2.2', 'X2.3_customChoiceValue', 'X2.4',
                   'X2.6', 'X3.1', 'X3.2', 'X3.2.01', 'X3.3', 'X3.4', 'X3.5', 'X4.1', 'X4.1.01', 'X4.2',
                   'X4.3')
  hardcode_vars <- c('X1.1', 'X1.3', 'X1.4', 'X1.5', 'X1.6', 'X1.7', 'X1.8')
  
  factor_inds <- which(!(names(df) %in% c(continuous_vars, hardcode_vars)))
  continuous_inds <- which(!(names(df) %in% c(factor_vars, hardcode_vars)))
  
  df[ , factor_inds] <- lapply(df[ , factor_inds], factor)
  df[ , continuous_inds] <- lapply(df[ , continuous_inds], as.numeric)
  
  return(df)
}

accuracy_related_penalties_logic <- function(df){
  
  df <- accuracy_related_penalties_transform_data(df)
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, accuracy_related_penalties_factors, FALSE, answered_questions)
  
  if (df$X1.1 == LACKING_ECONOMIC_SUBSTANCE){
    pred_label <- NO_RCGF
    pred_prob <- 0
  } else if (df$X1.3 == 'No'){
    pred_label <- NO_RCGF
    pred_prob <- 0
  } else if (df$X1.4 == 'Yes'){
    pred_label <- RCGF
    pred_prob <- 1
  } else if (df$X1.5 == 'No'){
    pred_label <- NO_RCGF
    pred_prob <- 0
  } else if (df$X1.6 == CHARITABLE_DEDUCTION_PROPERTY){
    pred_label <- NO_RCGF
    pred_prob <- 0
  } else if (df$X1.7 == 'Yes'){
    pred_label <- RCGF
    pred_prob <- 1
  } else if (df$X1.8 == 'No'){
    pred_label <- NO_RCGF
    pred_prob <- 0
  } else {
    pred_label <- '__noHardcode__'
    pred_prob <- NA
  }
 
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

accuracy_related_penalties_predict <- function(df){
  
  df <- accuracy_related_penalties_transform_data(df)
  
  hardcodes <- accuracy_related_penalties_logic(df)
  
  if (hardcodes$prediction == RCGF | hardcodes$prediction == NO_RCGF){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(accuracy_related_penalties_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, RCGF, NO_RCGF)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}



accuracy_related_penalties_interim <- function(df){
  
  df <- accuracy_related_penalties_transform_data(df)
  
  hardcodes <- accuracy_related_penalties_logic(df)
  
  if (hardcodes$prediction == RCGF | hardcodes$prediction == NO_RCGF){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
    
  } else {
    return(create_interim_predictor(accuracy_related_penalties_model,
                                    NO_RCGF,
                                    RCGF,
                                    data.frame(),
                                    1)(df))
  }
}
