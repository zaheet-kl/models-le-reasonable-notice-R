prophetSource('interim_analysis', 'interim_analysis.R')

leave_of_absence_model <- readRDS(prophetPath('models', 'usEmploymentForesight', 'leaveOfAbsence', 'dala_lr_v9.rds'))

TRIABLE_ISSUE <- 'Triable Issue'
NOT_TRIABLE <- 'Not Triable'

NO <- 'No'
NEITHER <- 'Neither'

leave_of_absence_factors <- function(){
  return(c(
    '1.1',
    '1.2',
    '1.2x',
    '1.4_customChoiceValue',
    '2.1_customChoiceValue',
    '2.1.1',
    '2.1.2',
    '2.1.3',
    '2.1.1.1',
    '2.2',
    '2.4',
    '2.7',
    '3.1',
    '3.1.2',
    '3.2_customChoiceValue',
    '3.2.1', 
    '3.3',
    '3.3.2',
    '3.3x',
    '3.4.1',
    '3.5',
    '3.6',
    '4.1',
    '4.1.1',
    '4.1.2',
    '4.1.3',
    '4.1.4',
    '4.2',
    '4.3',
    '4.4.1',
    '4.4.2',
    '4.5',
    '4.6_customChoiceValue',
    '4.7',
    '4.8',
    '5.1',
    '5.1.2',
    '5.2',
    '5.4',
    '5.5',
    '5.6'
  ))
}

physical_category_factors_dala <- function(df){
  
  df$Xmusculoskeletal <- 'false'
  df$Xneurological <- 'false'
  df$Xautoimmune <- 'false'
  df$Xphysical_other <- 'false'
  df$Xphysical_none <- 'false'
  
  for (i in seq(1,nrow(df))){
    
    disabilities_list <- as.character(df$X2.1.2[i])
    disabilities <- unlist(strsplit(disabilities_list, ';'))
    
    if (identical(disabilities, character(0)) |
        any(grepl('default', disabilities))){
      df$Xphysical_none[i] <- 'true'
    }
    
    if (any(grepl('Musculoskeletal Injury', disabilities))){
      df$Xmusculoskeletal[i] <- 'true'
    } 
    
    if (any(grepl('Neurological', disabilities))){
      df$Xneurological[i] <- 'true'
    } 
    
    if (any(c('Cancer', 'Autoimmune Disorder') %in% disabilities)){
      df$Xautoimmune[i] <- 'true'
    }
    
    if (!any(grepl('Musculoskeletal Injury', disabilities)) &
        !any(grepl('Neurological', disabilities)) &
        !any(c('Cancer', 'Autoimmune Disorder') %in% disabilities) &
        !identical(disabilities, character(0))){
      df$Xphysical_other[i] <- 'true'
    }
  }
  return(df)
}

mental_category_factors_dala <- function(df){
  
  df$Xalcoholism <- 'false'
  df$Xanxiety <- 'false'
  df$Xmental_other <- 'false'
  df$Xmental_none <- 'false'
  
  for (i in seq(1,nrow(df))){
    
    disabilities_list <- as.character(df$X2.1.3[i])
    disabilities <- unlist(strsplit(disabilities_list, ';'))
    
    if (any(c('Alcoholism') %in% disabilities)){
      df$Xalcoholism[i] <- 'true'
    } 
    
    if (any(c('Anxiety', 'Depression') %in% disabilities)){
      df$Xanxiety[i] <- 'true'
    } 
    
    if (!any(c('Anxiety', 'Depression', 'Alcoholism') %in% disabilities)){
      df$Xmental_other[i] <- 'true'
    }
    
    if (identical(disabilities, character(0)) |
        any(grepl('default', disabilities))){
      df$Xmental_none[i] <- 'true'
    }
  }
  return(df)
}

leave_of_absence_transform_data <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, leave_of_absence_factors, FALSE, answered_questions)
  
  df <- format_dataframe_numerics(df, c('X1.2', 'X3.6', 'X3.1.2', 'X3.2.1', 'X3.3.2'))

  # set total prior days as zero for a new requestion
  df$X3.1.2[which(is.na(df$X3.1.2) & df$X3.1 == 'New request')] <- 0
  
  # map 2.1.1.1 to 2.1 if 2.1==Neither
  if (df$X2.1.1.1 == 'Mental condition'){
    df$X2.1_customChoiceValue <- 'Mental'
  } else if (df$X2.1.1.1 == 'Physical condition'){
    df$X2.1_customChoiceValue <- 'Physical'
  } else if (df$X2.1.1.1 == 'Both mental and physical conditions'){
    df$X2.1_customChoiceValue <- 'Both'
  }
  
  # 20 day cutoff for employer's quick decision to termination
  df$X3.6_cutoff <- df$X3.6 < 20
  df$X3.6_cutoff[which(df$X3.6_cutoff)] <- 'true'
  df$X3.6_cutoff[which(df$X3.6_cutoff != 'true')] <- 'false'
  
  # total leave days asked for minus statutory requirements
  df$Xtra <- df$X3.1.2 + df$X3.2.1 - df$X3.3.2
  df$Xtra <- log(df$Xtra)
  df$Xtra[which(is.na(df$Xtra))] <- 1
  
  # combine approximate date and firm date into one
  if (df$X3.2_customChoiceValue == 'Neither'){
    df$X3.2date <- 'no_date'
  } else if (df$X3.2_customChoiceValue != 'Neither'){
    df$X3.2date <- 'date'
  }
  
  # create factor for interaction between total days and date given by employee
  df$Xdate_xtradays <- df$Xtra
  df$Xdate_xtradays[which(df$X3.2date == 'no_date')] <- 0
  
  # for when the employee does not give a firm or approximate date
  df$Xnodate_xtradays <- df$Xtra
  df$Xnodate_xtradays[which(df$X3.2date == 'date')] <- 0
  df$Xnodate_xtradays[which(df$Xnodate_xtradays == 0)] <- 'No'
  df$Xnodate_xtradays[which(df$Xnodate_xtradays == 1)] <- 'Yes'
  
  # custom value for jurisdiction 1.2x
  if (df$X1.2x == 'California'){
    df$X1.2x_custom = 'California'
  } else if (df$X1.2x != 'California'){
    df$X1.2x_custom = 'Other'
  }
  
  # transform multi-select dropdown to factors
  df <- physical_category_factors_dala(df)
  df <- mental_category_factors_dala(df)
  
  df <- empty_question_transformation(df, leave_of_absence_factors, TRUE, answered_questions)
  
  return(df)
}

leave_of_absence_logic <- function(df){
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, leave_of_absence_factors, FALSE, answered_questions)
  
  if (df$X3.4.1 == NO){
    pred_prob <- 1
    pred_label <- TRIABLE_ISSUE
  } else if (df$X3.3 == NO){
    pred_prob <- 1
    pred_label <- TRIABLE_ISSUE
  } else if (df$X2.1_customChoiceValue == NEITHER & df$X2.1.1 == NO){
    pred_prob <- 1
    pred_label <- NOT_TRIABLE
  } else {
    pred_prob <- NA
    pred_label <- '__noHardcode__'
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

leave_of_absence_predict <- function(df){

  df <- leave_of_absence_transform_data(df)
  hardcodes <- leave_of_absence_logic(df)
  
  if(hardcodes$prediction == TRIABLE_ISSUE |
     hardcodes$prediction == NOT_TRIABLE){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
  } else {
    pred_prob <- predict(leave_of_absence_model, df, type='response')
    pred_label <- ifelse(pred_prob>0.5, TRIABLE_ISSUE, NOT_TRIABLE)
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
  
}

leave_of_absence_interim <- function(df){
  
  df <- leave_of_absence_transform_data(df)
  
  hardcodes <- leave_of_absence_logic(df)
  
  if(hardcodes$prediction == TRIABLE_ISSUE |
     hardcodes$prediction == NOT_TRIABLE){
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
  } else {
    return(create_interim_predictor(leave_of_absence_model,
                                    NOT_TRIABLE,
                                    TRIABLE_ISSUE,
                                    data.frame(),
                                    1)(df))
  
  }
}


