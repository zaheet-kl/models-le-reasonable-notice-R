prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

innocent_spouse_knowledge_model <- readRDS(prophetPath('models', 'usTaxForesight', 'innocentSpouse', 'is_know_v2.rds'))
innocent_spouse_fairness_model <- readRDS(prophetPath('models', 'usTaxForesight', 'innocentSpouse', 'is_fair_v1.rds'))

CONSTRUCTIVE_KNOWLEDGE <- 'Constructive Knowledge'
NO_CONSTRUCTIVE_KNOWLEDGE <- 'No constructive knowledge'

LIABILITY_IS_FAIR <- 'Liability is fair'
LIABILITY_IS_UNFAIR <- 'Liability is unfair'

INNOCENT_SPOUSE_RELIEF <- 'Innocent spouse relief'
SEPARATION_OF_LIABILITY <- 'Separation of liability'
EQUITABLE_RELIEF <- 'Equitable relief'
TRADITIONAL_RELIEF <- 'Traditional § 66(c) relief'
NO_RELIEF <- 'No relief'

YES_PURSUANT_TO_A_AND_B = 'Yes, innocent spouse relief pursuant to §§ 6015(a) and (b)'
YES_PURSUANT_TO_C_AND_D = 'Yes, separation of liability pursuant to §§ 6015(c) and (d)'
YES_PURSUANT_TO_F = 'Yes, equitable relief pursuant to § 6015(f)'
YES_PURSUANT_TO_C_AND_A = 'Yes, traditional § 66(c) relief pursuant to §§ 66(c) and 879(a)'
YES_PURSUANT_TO_C4 = 'Yes, equitable relief pursuant to § 66(c)(4)'

MARRIED = 'Still married and living together'
DIVORCED = 'Divorced'
SEPARATED_NOT_TEMPORARILY = 'Separated, not temporarily'
WIDOWED = 'Widowed'

INCOME_OR_PROPERTY_FROM_SPOUSE = "The income tax liability arose from the non-requesting spouse's own income or property"
INCOME_OR_PROPERTY_ATTRIBUTABLE_THROUGH_LAW = 'The income tax liability arose from income or property that was attributable to the person solely through the operation of community property law'
INCOME_OR_PROPERTY_CONTROLLED_BY_SPOUSE = 'The income tax liability arose from a property that the person held in name only and that was actually controlled by the non-requesting spouse'
INCOME_OR_PROPERTY_SOLELY_OWNED = 'There is no connection, the income tax liability arose from income or property that was the person’s alone'
INCOME_OR_PROPERTY_OWNED_BY_BOTH = 'The income tax liability arose from income that both spouses earned, or property that both spouses owned'

UNDERPAYMENT_OF_TAX = 'Underpayment of tax'
NO = 'No'
YES = 'Yes'
ERRONEOUS_ITEM_BELONGING_TO_PERSON = 'An erroneous item belonging to the person'
ERRONEOUS_ITEM_BELONGING_TO_SPOUSE = 'An erroneous item belonging to the non-requesting spouse'
NONE_OF_THE_ABOVE = 'None of the above'

innocent_spouse_factors <- function(){
  logic_qs <- c('1.2', '1.3', '1.3.01', '1.3.02', '1.3.03', '1.3.04', '1.3.05', '1.3.06',
                '1.4_customChoiceValue', '2.1', '3.1_customChoiceValue', '4.5')
  
  fairness_qs <- attributes(innocent_spouse_fairness_model$terms)$term.labels
  fairness_qs <- gsub('^X', '', fairness_qs)
  
  knowledge_qs <- attributes(innocent_spouse_knowledge_model$terms)$term.labels
  knowledge_qs <- gsub('^X', '', knowledge_qs)
  
  factors <- c(logic_qs, fairness_qs, knowledge_qs)
  factors <- factors[!duplicated(factors)]
  
  return(factors)
}

innocent_spouse_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, innocent_spouse_factors, FALSE, answered_questions)
  
  continuous_vars <- c('X2.1', 'X2.3', 'X2.8', 'X2.8.01', 'X2.10', 'X2.11', 'X3.2', 'X4.4')
  factor_vars <- c('X1.1_customChoiceValue', 'X1.4_customChoiceValue', 'X2.2_customChoiceValue',
                   'X2.4_customChoiceValue', 'X2.5', 'X2.6_customChoiceValue', 'X2.7', 'X2.9',
                   'X3.1_customChoiceValue', 'X3.3', 'X3.4', 'X3.5', 'X4.1', 'X4.2', 'X4.3', 'X4.5')
  hardcode_vars <- c('X1.2', 'X1.3', 'X1.3.01', 'X1.3.02', 'X1.3.03', 'X1.3.04', 'X1.3.05', 'X1.3.06',
                     'X1.4', 'X2.1', 'X3.1', 'X4.5')

  factor_inds <- which(!(names(df) %in% c(continuous_vars, hardcode_vars)))
  continuous_inds <- which(!(names(df) %in% c(factor_vars, hardcode_vars)))
  
  df[ , factor_inds] <- lapply(df[ , factor_inds], factor)
  df[ , continuous_inds] <- lapply(df[ , continuous_inds], as.numeric)
  
  # make the UNANSWERED values NULL again
  df <- empty_question_transformation(df, innocent_spouse_factors, TRUE, answered_questions)
  
  return(df)
}


innocent_spouse_relief <- function(df, fairness_outcome, knowledge_outcome){
  
  if ((df$X1.2 == YES_PURSUANT_TO_A_AND_B | (df$X1.3.03 == ERRONEOUS_ITEM_BELONGING_TO_SPOUSE & df$X1.3.05 == YES)) &
      df$X2.1 == 0 &
      knowledge_outcome == NO_CONSTRUCTIVE_KNOWLEDGE &
      fairness_outcome == LIABILITY_IS_UNFAIR){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

traditional_relief <- function(df, fairness_outcome, knowledge_outcome){
  
  if ((df$X1.2 == YES_PURSUANT_TO_C_AND_A | df$X1.3.06 == YES) &
      df$X2.1 == 0 &
      knowledge_outcome == NO_CONSTRUCTIVE_KNOWLEDGE &
      fairness_outcome == LIABILITY_IS_UNFAIR){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

equitable_relief_no_sl_overlap <- function(df, fairness_outcome, knowledge_outcome){
  
  if ((df$X1.2 == YES_PURSUANT_TO_F | df$X1.3.01 == UNDERPAYMENT_OF_TAX | df$X1.3.05 == NO) &
      fairness_outcome == LIABILITY_IS_UNFAIR){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

equitable_relief_66 <- function(df, fairness_outcome, knowledge_outcome){
  
  if ((df$X1.2 == YES_PURSUANT_TO_C4 | 
       (df$X1.3 == NO & 
        (df$X1.3.02 == NO | df$X1.3.04 == NONE_OF_THE_ABOVE | df$X1.3.06 == NO))) &
      fairness_outcome == LIABILITY_IS_UNFAIR){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

equitable_relief_joint_return <- function(df, fairness_outcome, knowledge_ouctome){
  
  if (df$X1.3.03 == ERRONEOUS_ITEM_BELONGING_TO_PERSON &
      (df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_SOLELY_OWNED | df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_OWNED_BY_BOTH) &
      fairness_outcome == LIABILITY_IS_UNFAIR){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

separation_of_liability_knowledge <- function(df, fairness_outcome, knowledge_outcome){
  
  if ((df$X1.2 == YES_PURSUANT_TO_C_AND_D |
       (df$X1.3.05== YES & df$X1.4_customChoiceValue == DIVORCED)) &
      df$X2.1 == 0 &
      (df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_FROM_SPOUSE |
       df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_ATTRIBUTABLE_THROUGH_LAW |
       df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_CONTROLLED_BY_SPOUSE)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

separation_of_liability_abuse <- function(df, fairness_outcome, knowledge_outcome){
  
  if ((df$X1.2 == YES_PURSUANT_TO_C_AND_D | df$X1.2 == YES_PURSUANT_TO_A_AND_B |
       (df$X1.3.05== YES & df$X1.4_customChoiceValue == DIVORCED)) &
      df$X2.1 != 0 &
      df$X4.5 == YES &
      (df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_FROM_SPOUSE |
       df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_ATTRIBUTABLE_THROUGH_LAW |
       df$X3.1_customChoiceValue == INCOME_OR_PROPERTY_CONTROLLED_BY_SPOUSE)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

no_relief <- function(df, fairness_outcome, knowledge_outcome){
  
  if (fairness_outcome == LIABILITY_IS_FAIR){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

innocent_spouse_logic <- function(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob){
  
  fairness_prob_above_50 <- convert_probability_above_50(fairness_prob)
  knowledge_prob_above_50 <- convert_probability_above_50(knowledge_prob)
  
  answered_questions <- names(df)
  df <- empty_question_transformation(df, innocent_spouse_factors, FALSE, answered_questions)
  
  if (innocent_spouse_relief(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=INNOCENT_SPOUSE_RELIEF,
                pred_prob=(fairness_prob_above_50 + knowledge_prob_above_50)/2))
    
  } else if (traditional_relief(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=TRADITIONAL_RELIEF,
                pred_prob=(fairness_prob_above_50 + knowledge_prob_above_50)/2))
    
  } else if (equitable_relief_no_sl_overlap(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=EQUITABLE_RELIEF,
                pred_prob=fairness_prob_above_50))
    
  } else if (equitable_relief_66(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=EQUITABLE_RELIEF,
                pred_prob=fairness_prob_above_50))
    
  } else if (equitable_relief_joint_return(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=EQUITABLE_RELIEF,
                pred_prob=fairness_prob_above_50))
    
  } else if (separation_of_liability_knowledge(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=SEPARATION_OF_LIABILITY,
                pred_prob=1))
    
  } else if (separation_of_liability_abuse(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=SEPARATION_OF_LIABILITY,
                pred_prob=1))
    
  } else if (no_relief(df, fairness_outcome, knowledge_outcome)){
    return(list(pred_label=NO_RELIEF,
                pred_prob=fairness_prob_above_50))
    
  } else {
    return(list(pred_label=EQUITABLE_RELIEF,
                pred_prob=fairness_prob_above_50))
  }
  
}

innocent_spouse_predict <- function(df){
  
  df <- innocent_spouse_transform_data(df)

  fairness_prob <- predict(innocent_spouse_fairness_model, df, type = 'response')
  fairness_outcome <- ifelse(fairness_prob > 0.5, LIABILITY_IS_UNFAIR, LIABILITY_IS_FAIR)
  
  knowledge_prob <- predict(innocent_spouse_knowledge_model, df, type = 'response')
  knowledge_outcome <- ifelse(knowledge_prob > 0.5, NO_CONSTRUCTIVE_KNOWLEDGE, CONSTRUCTIVE_KNOWLEDGE)
  
  results <- innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob)
  
  pred_prob <- results$pred_prob
  pred_label <- results$pred_label
  
  return(list(
    prediction = pred_label,
    probability = pred_prob,
    subResults = list(
      list(
        group = 'Fairness',
        prediction = fairness_outcome,
        probability=fairness_prob
      ),
      list(
        group = 'Constructive_Knowledge',
        prediction = knowledge_outcome,
        probability=knowledge_prob
      )
    )
  ))
  
}

innocent_spouse_interim <- function(df){
  
  df <- innocent_spouse_transform_data(df)
  
  innocent_spouse_fairness_interim <- create_interim_predictor(innocent_spouse_fairness_model,
                                                               LIABILITY_IS_FAIR,
                                                               LIABILITY_IS_UNFAIR,
                                                               data.frame(),
                                                               2.5)(df)
  
  fairness_prob <- innocent_spouse_fairness_interim$probability
  fairness_outcome <- innocent_spouse_fairness_interim$prediction

  innocent_spouse_knowledge_interim <- create_interim_predictor(innocent_spouse_knowledge_model,
                                                                CONSTRUCTIVE_KNOWLEDGE,
                                                                NO_CONSTRUCTIVE_KNOWLEDGE,
                                                                data.frame(),
                                                                1.5)(df)
  
  knowledge_prob <- innocent_spouse_knowledge_interim$probability
  knowledge_outcome <- innocent_spouse_knowledge_interim$prediction
  
  results <- innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob)
  
  pred_prob <- results$pred_prob
  pred_label <- results$pred_label
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
  
}

