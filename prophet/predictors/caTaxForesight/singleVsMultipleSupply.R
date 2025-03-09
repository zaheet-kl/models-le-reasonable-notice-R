prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

single_vs_multiple_supply_model <- readRDS(prophetPath('models', 'caTaxForesight', 'singleVsMultipleSupply', 'svm_lr_v1.rds'))

single_vs_multiple_supply_constants <- data.frame(SINGLE_SUPPLY = 'Single supply',
                                                  MULTIPLE_SUPPLIES = 'Multiple supplies',
                                                  stringsAsFactors = FALSE)

single_vs_multiple_supply_factors <- function(){
  labels <- attributes(single_vs_multiple_supply_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  
  return(labels)  
}


single_vs_multiple_supply_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, single_vs_multiple_supply_factors, FALSE, answered_questions)
  
  if (df$X3.2 == 'No'){
    df$X3.2 <- factor('The elements are not all goods')
  }
  
  df <- empty_question_transformation(df, single_vs_multiple_supply_factors, TRUE, answered_questions)
  
  return(df)
}

single_vs_multiple_supply_predict <- function(df){
  
  df <- single_vs_multiple_supply_transform_data(df)
  
  pred_prob <- predict(single_vs_multiple_supply_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5,
                       single_vs_multiple_supply_constants$SINGLE_SUPPLY,
                       single_vs_multiple_supply_constants$MULTIPLE_SUPPLIES)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

single_vs_multiple_supply_interim <- function(df){
  
  df <- single_vs_multiple_supply_transform_data(df)
  
  return(create_interim_predictor(single_vs_multiple_supply_model,
                                  single_vs_multiple_supply_constants$MULTIPLE_SUPPLIES,
                                  single_vs_multiple_supply_constants$SINGLE_SUPPLY,
                                  data.frame(),
                                  1)(df))
}
