prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

progressive_discipline_susp_1v30_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'progressiveDiscipline', 'suspension_1v30_model_v2.rds'))
progressive_discipline_susp_full_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'progressiveDiscipline', 'suspension_full_model_v2.rds'))
progressive_discipline_term_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'progressiveDiscipline', 'termination_not_termination_model_v2.rds'))
progressive_discipline_warn_model <- readRDS(prophetPath('models', 'caEmploymentForesight', 'progressiveDiscipline', 'warning_not_warning_model_v2.rds'))

progressive_discipline_factors <- function(){
  
  labels <- c('1.2', '1.3', '1.4', '1.6_customChoiceValue', '1.7', '1.8', '1.9',
  '2.1.1', '2.1.2', '2.1.3', '2.1.4', '2.1.5',
  '2.1.6', '2.1.7', '2.1.8', '2.1.9', '2.1.10',
  '2.1.11', '2.1.12', '2.1.13', '2.1.14', '2.1.15',
  '2.1.16', '2.1.17', '2.1.18', '2.1.19', '2.2_1',
  '2.2.1', '2.2.2', '2.2.3', '2.2.4', '2.2.5', '2.2.6', 
  '2.2.7', '2.2.8', '2.2.9', '2.3', '2.4', '2.5', '2.6',
  '2.7', '2.8', '2.9', '2.10_1', '2.11', '2.12',
  '3.1', '3.1.1', '3.1.2', '3.2', '3.3', '3.4',
  '4.1', '4.1.1_customChoiceValue', '4.2_customChoiceValue', '4.2.1_customChoiceValue', '4.2.2', '4.2.3', '4.3',
  '5.1', '5.2.1', '5.3', '4.2.1.1')
  
  return(labels)
}

progressive_discipline_transform_data <- function(df){
  
  colnames(df) <- gsub('_customChoiceValue', '', colnames(df))
  
  df <- format_dataframe_numerics(df, c("X1.4", "X4.2.2", "X4.2.3", "X4.2.1.1"))
  
  df$X4.2.1[which(is.na(df$X4.2.1)==TRUE)] <- 'No discipline'

  levels(df$X1.6) <- c(levels(df$X1.6),  c('Technical / skilled trades', 'Middle/Lower Management'))
  df$X1.6[which(df$X1.6=='Technical')] <- 'Technical / skilled trades'
  df$X1.6[which(df$X1.6=='Upper Management')] <- 'Middle/Lower Management'
  
  df$X1.4 <- df$X1.4/10
  
  # create custom columns for transformed variables
  custom_cols <- c('X4.2.1_custom', 'X4.1.1_custom',
                   'rehab_2.9', 'rehab_2.10_1', 'rehab_2.11', 'rehab_2.12', 'rehab_total', 'X4.2_custom')
  df[, custom_cols] <- NA
  
  # create custom logical variables for discipline types
  custom_logical <- c('X2.1_group1', 'X2.1_group2', 'X2.1_group3', 'X2.1_group4', 'X2.1_group5', 'X2.1_group6',
                      'X2.1_group7', 'X2.2_group1', 'X2.2_group2', 'X2.2_group3', 'X2.2_group4')
  
  df[, custom_logical] <- FALSE
  
  df$X2.1_group1[which(df$X2.1.1==T | df$X2.1.7==T | df$X2.1.15==T)] <- TRUE
  df$X2.1_group2[which(df$X2.1.5==T | df$X2.1.2==T)] <- TRUE
  df$X2.1_group3[which(df$X2.1.16==T | df$X2.1.17==T)] <- TRUE
  df$X2.1_group4[which(df$X2.1.3==T | df$X2.1.19==T | df$X2.1.18==T)] <- TRUE
  df$X2.1_group5[which(df$X2.1.6==T | df$X2.1.14==T | df$X2.1.12==T)] <- TRUE
  df$X2.1_group6[which(df$X2.1.13==T | df$X2.1.11==T | df$X2.1.8==T)] <- TRUE
  df$X2.1_group7[which(df$X2.1.9==T | df$X2.1.10==T)] <- TRUE
  
  df[c('X2.1_group1')] <- lapply(df[c('X2.1_group1')], as.factor)
  df[c('X2.1_group2')] <- lapply(df[c('X2.1_group2')], as.factor)
  df[c('X2.1_group3')] <- lapply(df[c('X2.1_group3')], as.factor)
  df[c('X2.1_group4')] <- lapply(df[c('X2.1_group4')], as.factor)
  df[c('X2.1_group5')] <- lapply(df[c('X2.1_group5')], as.factor)
  df[c('X2.1_group6')] <- lapply(df[c('X2.1_group6')], as.factor)
  df[c('X2.1_group7')] <- lapply(df[c('X2.1_group7')], as.factor)
  
  df$X2.2_group1[which(df$X2.2.1==T | df$X2.2.8==T | df$X2.2.5==T)] <- TRUE
  df$X2.2_group2[which(df$X2.2.2==T | df$X2.2.3==T | df$X2.2.4==T | df$X2.2.7==T)] <- TRUE
  df$X2.2_group3[which(df$X2.2.6==T)] <- TRUE
  df$X2.2_group4[which(df$X2.2.9==T | df$X2.2.4==T)] <- TRUE
  
  df[c('X2.2_group1')] <- lapply(df[c('X2.2_group1')], as.factor)
  df[c('X2.2_group2')] <- lapply(df[c('X2.2_group2')], as.factor)
  df[c('X2.2_group3')] <- lapply(df[c('X2.2_group3')], as.factor)
  df[c('X2.2_group4')] <- lapply(df[c('X2.2_group4')], as.factor)
  
  df$rehab_2.9[which(df$X2.9 == 'Yes')] <- 1
  df$rehab_2.9[which(df$X2.9 == 'No')] <- 0
  df$rehab_2.10_1[which(df$X2.10_1 == 'Yes')] <- 1
  df$rehab_2.10_1[which(df$X2.10_1 == 'No')] <- 0
  df$rehab_2.11[which(df$X2.11 == 'Yes')] <- 1
  df$rehab_2.11[which(df$X2.11 == 'No')] <- 0
  df$rehab_2.12[which(df$X2.12 == 'No')] <- 1
  df$rehab_2.12[which(df$X2.12 == 'Yes')] <- 0
  
  df$rehab_total <- df$rehab_2.9 + df$rehab_2.10_1 + df$rehab_2.11 + df$rehab_2.12
  
  df$X4.2_custom[which(df$X4.2=='The employee has a clean disciplinary record')] <- 'clean'
  df$X4.2_custom[which(df$X4.2=='Within a week')] <- 'not clean'
  df$X4.2_custom[which(df$X4.2=='Within a month')] <- 'not clean'
  df$X4.2_custom[which(df$X4.2=='Within six months')] <- 'not clean'
  df$X4.2_custom[which(df$X4.2=='Within a year')] <- 'not clean'
  df$X4.2_custom[which(df$X4.2=='Over a year')] <- 'not clean'
  
  
  df$X4.2.1_custom[which(df$X4.2.1=='Termination')] <- 'Termination'
  df$X4.2.1_custom[which(df$X4.2.1=='Paid suspension' |
                           df$X4.2.1=='Suspension with final warning' |
                           df$X4.2.1=='Unpaid suspension')] <- 'Suspension'
  df$X4.2.1_custom[which(df$X4.2.1=='Written warning' |
                           df$X4.2.1=='Verbal warning')] <- 'Warning'
  df$X4.2.1_custom[which(df$X4.2.1=='No discipline')] <- 'No discipline'
  df$X4.2.1_custom[which(df$X4.2.1=='Final warning')] <- 'Other'
  df$X4.2.1_custom[which(df$X4.2.1=='Other')] <- 'Other'
  df$X4.2.1_custom[which(df$X4.2.1=='Temporary demotion')] <- 'Other'
  
  df$X4.1.1_custom[which(df$X4.1.1=='Termination')] <- 'Termination'
  df$X4.1.1_custom[which(df$X4.1.1=='Paid suspension' |
                           df$X4.1.1=='Suspension with final warning' |
                           df$X4.1.1=='Unpaid suspension')] <- 'Suspension'
  df$X4.1.1_custom[which(df$X4.1.1=='Written warning' |
                           df$X4.1.1=='Verbal warning' |
                           df$X4.1.1=='No discipline')] <- 'Warning'
  df$X4.1.1_custom[which(df$X4.1.1=='The policy does not specify' |
                           df$X4.1.1=='Other' |
                           df$X4.1.1=='Final warning' |
                           df$X4.1.1=='Temporary demotion')] <- 'The policy does not specify'
  
  # Convert 2.1 to count checkbox
  col_1 <- which(colnames(df)=='X2.1.1')
  col_2 <- which (colnames(df)=='X2.1.19')
  df[,col_1:col_2] <- mutate_all(df[,col_1:col_2], .funs=as.logical)
  df[c('X2.1_count')] <- apply(df[,col_1:col_2],1,function(x) sum(x))

  
  # Convert 2.2 to count checkbox
  col_1 <- which(colnames(df)=='X2.2.1')
  col_2 <- which (colnames(df)=='X2.2.9')
  df[,col_1:col_2] <- mutate_all(df[,col_1:col_2], .funs=as.logical)
  df[c('X2.2_count')] <- apply(df[,col_1:col_2],1,function(x) sum(x))
  
  df[c('X4.2.1_custom')] <- lapply(df[c('X4.2.1_custom')], as.factor)
  df[c('X4.1.1_custom')] <- lapply(df[c('X4.1.1_custom')], as.factor)
  df[c('X4.2_custom')] <- lapply(df[c('X4.2_custom')], as.factor)
  
  return(df)
}

progressive_discipline_constants <- data.frame(TERMINATION='Termination',
                                               NOT_TERMINATION='Not Termination',
                                               WARNING='Warning',
                                               NOT_WARNING='Not Warning',
                                               SUSPENSION='Suspension',
                                               ONE_DAY='1 day',
                                               ONE_DAY_TO_ONE_WEEK='1 day - 1 week',
                                               ONE_TO_TWO_WEEKS='1 - 2 weeks',
                                               TWO_PLUS_WEEKS='2+ weeks',
                                               ONE_PLUS_WEEKS='1+ weeks',
                                               UP_TO_ONE_WEEK='Up to 1 week',
                                               stringsAsFactors = FALSE)

progressive_discipline_round_probabilities <- function(preds, pred_order){
  
  if (rowSums(preds)==100){
    # if sum of rounded probabilities is 100 then continue
    return(preds)
    
  } else if(rowSums(preds)==99){
    
    # if sum of rounded probabilities is 99 then add 1 to max probability
    preds[which.max(preds)] <- preds[which.max(preds)] + 1
    
  } else if(rowSums(preds)==101){
    
    if (length(unique(preds))==3){
      
      # when sum is 101 take a percent away from second highest prob
      preds[pred_order[1,2]] <- preds[pred_order[1,2]] - 1
      
    } else if(preds[pred_order[1,1]] == preds[pred_order[1,2]]){
      
      # when sum is 101 and the highest and second highest prob are equal take a percent away from second highest prob
      preds[pred_order[1,2]] <- preds[pred_order[1,2]] - 1
      
    } else if(preds[pred_order[1,2]] == preds[pred_order[1,3]]){
      
      # when sum is 101 and the second and third highest prob are equal take a percent away from third highest prob
      preds[pred_order[1,3]] <- preds[pred_order[1,3]] - 1
      
    }
  }

  return(preds)
}

progressive_discipline_order_probabilities <- function(warn_prob, susp_prob, term_prob, df, pred_label){
  
  preds <- data.frame(warn_prob, susp_prob, term_prob)
  
  preds <- preds*100
  preds <- round(preds, 0)
  
  preds$warn_prob <- as.integer(preds$warn_prob)
  preds$susp_prob <- as.integer(preds$susp_prob)
  preds$term_prob <- as.integer(preds$term_prob)
  
  ordered_inds <- order(c(preds$warn_prob, preds$susp_prob, preds$term_prob), decreasing = TRUE)

  # ordered_inds <- order(preds, decreasing=TRUE)
  pred_order <- data.frame(ordered_inds[1], ordered_inds[2], ordered_inds[3])
  preds <- progressive_discipline_round_probabilities(preds, pred_order)
  preds <- preds / 100

  prediction_df <- data.frame(pred_label, preds$warn_prob, preds$susp_prob, preds$term_prob, df$susp_prediction)
  colnames(prediction_df) <- c('pred_label', 'warn_prob', 'susp_prob', 'term_prob', 'susp_prediction')
  prediction_df[c('susp_prediction')] <- lapply(prediction_df[c('susp_prediction')], as.character)
  
  return(prediction_df)
}

progressive_discipline_suspension_logic <- function(prediction_df, df){
  
  if (prediction_df$pred_label == progressive_discipline_constants$SUSPENSION &
      (prediction_df$term_prob > 0.3)){
    prediction_df$susp_prediction <- progressive_discipline_constants$ONE_PLUS_WEEKS
  }
  
  if (prediction_df$pred_label == progressive_discipline_constants$SUSPENSION &
      prediction_df$warn_prob>0.3){
    prediction_df$susp_prediction <- progressive_discipline_constants$UP_TO_ONE_WEEK
  }
  
  # only do hardcode if 4.2.1 is suspension
  if (!is.na(df$X4.2.1.1)){
    # previous is 1 day - 1 week and prediction is 1 day
    if (df$X4.2.1.1 > 1 &
        df$X4.2.1.1 < 5 &
        prediction_df$susp_prediction == progressive_discipline_constants$ONE_DAY){
      prediction_df$susp_prediction <- progressive_discipline_constants$ONE_DAY_TO_ONE_WEEK
      
      # previous is 1 - 2 weeks
    } else if (df$X4.2.1.1 >= 5 &
               df$X4.2.1.1 <= 9 &
               (prediction_df$susp_prediction == progressive_discipline_constants$ONE_DAY |
                prediction_df$susp_prediction == progressive_discipline_constants$ONE_DAY_TO_ONE_WEEK |
                prediction_df$susp_prediction == progressive_discipline_constants$UP_TO_ONE_WEEK)){
      prediction_df$susp_prediction <- progressive_discipline_constants$ONE_TO_TWO_WEEKS
    } else if (df$X4.2.1.1 >= 10 &
               (prediction_df$susp_prediction == progressive_discipline_constants$ONE_DAY |
                prediction_df$susp_prediction == progressive_discipline_constants$ONE_DAY_TO_ONE_WEEK |
                prediction_df$susp_prediction == progressive_discipline_constants$ONE_TO_TWO_WEEKS |
                prediction_df$susp_prediction == progressive_discipline_constants$UP_TO_ONE_WEEK |
                prediction_df$susp_prediction == progressive_discipline_constants$ONE_PLUS_WEEKS)){
      prediction_df$susp_prediction <- progressive_discipline_constants$TWO_PLUS_WEEKS
    }
  }
  
  return(prediction_df)
}

progressive_discipline_predict <- function(df){
  
  df <- progressive_discipline_transform_data(df)

  # Termination Prediction
  term_predictions <- predict(progressive_discipline_term_model, newdata=df, type='response')
  term_label <- ifelse(term_predictions > 0.5,
                       progressive_discipline_constants$TERMINATION,
                       progressive_discipline_constants$NOT_TERMINATION)
  
  
  # Warning Prediction
  warn_predictions <- predict(progressive_discipline_warn_model, newdata=df, type='response')
  warn_label <- ifelse(warn_predictions>0.5,
                       progressive_discipline_constants$WARNING,
                       progressive_discipline_constants$NOT_WARNING)
  
  # calculate individual bin probabilities
  term_prob <- term_predictions * (1 - warn_predictions)
  warn_prob <- warn_predictions * (1 - term_predictions)
  susp_prob <- (1 - term_prob - warn_prob)
  
  
  # Label Prediction
  pred_label <- apply(cbind(warn_prob, susp_prob, term_prob), 1, which.max)
  
  # Label Assignment
  pred_label[which(pred_label==1)] <- progressive_discipline_constants$WARNING
  pred_label[which(pred_label==2)] <- progressive_discipline_constants$SUSPENSION
  pred_label[which(pred_label==3)] <- progressive_discipline_constants$TERMINATION
  pred_label <- as.factor(pred_label)
  
  # Add predicted probabilities to data frame
  df[,c('term_pred')] <- NA
  df[,c('term_pred')] <- term_predictions
  
  df[,c('warn_pred')] <- NA
  df[,c('warn_pred')] <- warn_predictions
  
  # single day vs not single day prediction
  susp_1v30_pred_prob <- predict(progressive_discipline_susp_1v30_model, newdata=df, type='response')
  susp_1v30_pred <- apply(susp_1v30_pred_prob, 1, which.max)
  
  # full suspension prediction (4 bins)
  susp_full_pred_prob <- predict(progressive_discipline_susp_full_model, newdata=df, type='response')
  susp_full_pred <- apply(susp_full_pred_prob, 1, which.max)
  
  # add suspension labels
  df['susp_prediction'] <- NA
  df$susp_prediction[which(susp_1v30_pred==1)] <- progressive_discipline_constants$ONE_DAY
  df$susp_prediction[which(susp_1v30_pred==3)] <- progressive_discipline_constants$TWO_PLUS_WEEKS
  df$susp_prediction[which(susp_1v30_pred==2 & susp_full_pred==1)] <- progressive_discipline_constants$ONE_DAY
  df$susp_prediction[which(susp_1v30_pred==2 & susp_full_pred==2)] <- progressive_discipline_constants$ONE_DAY_TO_ONE_WEEK
  df$susp_prediction[which(susp_1v30_pred==2 & susp_full_pred==3)] <- progressive_discipline_constants$ONE_TO_TWO_WEEKS
  df$susp_prediction[which(susp_1v30_pred==2 & susp_full_pred==4)] <- progressive_discipline_constants$TWO_PLUS_WEEKS
  
  # round probabilities so they sum to 1
  prediction_df <- progressive_discipline_order_probabilities(warn_prob, susp_prob, term_prob, df, pred_label)
  
  prediction_df <- progressive_discipline_suspension_logic(prediction_df, df)
  
  return(list(
    type = 'progressive-discipline-prediction',
    prediction = pred_label,
    warningProbability = prediction_df$warn_prob,
    suspensionProbability = prediction_df$susp_prob,
    terminationProbability = prediction_df$term_prob,
    subResults = list(
      list(
        group = 'Suspension_Days',
        prediction = prediction_df$susp_prediction
      )
    )
  ))
}


