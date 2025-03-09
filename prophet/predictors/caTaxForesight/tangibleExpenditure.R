prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

tangible_expenditure_repair_model <- readRDS(prophetPath('models', 'caTaxForesight', 'tangibleExpenditure', 'ter_lr_v2.rds'))
tangible_expenditure_nonrepair_model <- readRDS(prophetPath('models', 'caTaxForesight', 'tangibleExpenditure', 'tenr_lr_v2.rds'))

YES <- 'Yes'
NO <- 'No'

CURRENT_TANGIBLE <- 'Current'
CAPITAL_TANGIBLE <- 'Capital'

tangible_expenditure_factors <- function(){
  
  labels_repair <- attributes(tangible_expenditure_repair_model$terms)$term.labels
  labels_repair <- gsub('^X', '', labels_repair)
  
  labels_nonrepair <- attributes(tangible_expenditure_nonrepair_model$terms)$term.labels
  labels_nonrepair <- gsub('^X', '', labels_nonrepair)
  
  labels <- c(labels_repair, labels_nonrepair, c('2.5'))
  labels <- labels[!duplicated(labels)]
  
  return(labels)
}

tangible_expenditures_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, tangible_expenditure_factors, FALSE, answered_questions)
  
  df$X2.1 <- as.character(df$X2.1)
  
  df$X2.1[df$X2.1 == 'Chattel (tangible personal property that is not affixed to real property, e.g., furniture, computer, etc.)'] <- 'Chattel'
  
  continuous_vars <- c('X1.3', 'X2.3', 'X4.1', 'X4.6', 'X4.7', 'X4.8')
  factor_vars <- c('X2.1', 'X2.4', 'X2.5', 'X3.1','X3.2', 'X3.3', 'X4.2', 'X4.3', 'X4.4', 'X4.5', 'X5.1', 'X5.2', 'X5.3')
  
  factor_inds <- which(!(names(df) %in% c(continuous_vars)))

  df[ , factor_inds] <- lapply(df[ , factor_inds], factor)
  df <- format_dataframe_numerics(df, continuous_vars)
  
  if (df$X2.5 == YES){
    levels(df$X2.1)[which(levels(df$X2.1)=='Inventory')] <- 'Other'
  }

  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, tangible_expenditure_factors, TRUE, answered_questions)
  
  return(df)
}

tangible_expenditure_predict <- function(df){
  
  df <- tangible_expenditures_transform_data(df)
  
  if (df$X2.5 == YES){
    tangible_model <- tangible_expenditure_repair_model
  } else if (df$X2.5 == NO){
    tangible_model <- tangible_expenditure_nonrepair_model
  }
  
  pred_prob <- predict(tangible_model, newdata=df, type='response')
  pred_label <- ifelse(pred_prob>0.5, CURRENT_TANGIBLE, CAPITAL_TANGIBLE)
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}
