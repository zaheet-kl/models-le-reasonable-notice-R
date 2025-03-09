# functions to create data frames for interim analysis
create_qa_combinations <- function(model){
  #########################################################################################################
  # df_qa: model to be used in IA.

  # returns: df with question answer pairs.
  #########################################################################################################


  questions_used <- attributes(model$terms)$term.labels
  data_class <- as.list(attributes(model$terms)$dataClasses)
  factor_levels <- model$xlevels


  coefs <- data.frame(names(model$coefficients), model$coefficients)
  rownames(coefs) <- NULL
  names(coefs) <- c('question', 'coefficient')
  coef_names <- names(model$coefficients)

  df <- data.frame()

  for (question in questions_used){

    if (data_class[question] == "factor"){

      for (factor in as.list(factor_levels[question][[1]])){

        df <- rbind(df, data.frame(question=question, factor=factor))

      }

    } else if (data_class[question] == "numeric"){

      df <- rbind(df, data.frame(question=question, factor='numeric'))

    } else if (data_class[question] == 'ordered'){
      
      for (factor in as.list(factor_levels[question[[1]]])){
      
        df <- rbind(df, data.frame(question=question, factor=paste('ordered*', factor, sep='')))
        
      }
    } else if (data_class[question] == 'logical'){
      
      for (logical in as.list(c('TRUE', 'FALSE'))){
        
        df <- rbind(df, data.frame(question=question, factor=paste('logical*', logical, sep='')))
      }
    }

  }

  return(df)

}

fill_coefs <- function(df_qa, model){
  #########################################################################################################
  # df_qa: data frame with question/answer pairs.

  # returns: intercept for IA and data frame containing the coefficients q/a pairs for IA.
  #########################################################################################################
  
  questions_used <- attributes(model$terms)$term.labels
  data_class <- as.list(attributes(model$terms)$dataClasses)
  factor_levels <- model$xlevels

  coefs <- data.frame(names(model$coefficients), model$coefficients)
  rownames(coefs) <- NULL
  names(coefs) <- c('question', 'coefficient')
  coef_names <- names(model$coefficients)

  fixed_coefs <- data.frame()
  intercept_sum <- coef(model)["(Intercept)"]
  names(intercept_sum) <- ''
  
  # control for no intercept
  if (is.na(intercept_sum)){
    intercept_sum <- 0
  }

  for (row_ind in seq(1,nrow(df_qa))){

    coefficient_val <- 0
    q_id <- as.character(df_qa$question[row_ind])

    # binary factors
    if (length(factor_levels[q_id][[1]]) == 2 & data_class[q_id] == 'factor'){

      if (paste(df_qa[row_ind,1], df_qa[row_ind,2], sep='') %in% coefs$question){
        
        full_coefficient_name <- gsub("([[:punct:]])", "\\\\\\1", paste(q_id,df_qa[row_ind,2],sep=''))

        coefficient_ind <- which(grepl(paste("^", full_coefficient_name, "$", sep=""), coefs$question)==TRUE)
        coefficient_val <- 0.5*coefs$coefficient[coefficient_ind]

        fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))

      } else {
        
        fitted_ind <- which(factor_levels[q_id][[1]] != df_qa[row_ind,2])
        fitted_value <- factor_levels[q_id][[1]][fitted_ind]

        coefficient_ind <- which(grepl(gsub("\\.","\\\\.", paste(q_id,fitted_value,sep='')), coefs$question)==TRUE)
        coefficient_val <- -0.5*coefs$coefficient[coefficient_ind]

        intercept_sum <- intercept_sum - coefficient_val

        fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))

      }


      # factor levels with more than 1 factor
    } else if (length(factor_levels[q_id][[1]]) > 2 & data_class[q_id] == 'factor'){

      if (paste(df_qa[row_ind,1], df_qa[row_ind,2], sep='') %in% coefs$question){

        full_coefficient_name <- gsub("([[:punct:]])", "\\\\\\1", paste(q_id,df_qa[row_ind,2],sep=''))

        coefficient_ind <- which(grepl(paste("^", full_coefficient_name, "$", sep=""), coefs$question)==TRUE)
        coefficient_val <- coefs$coefficient[coefficient_ind]
        
        fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))

      } else {

        coefficient_val <- 0
        fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))

      }

      # factor levels that are numeric
    } else if (df_qa$factor[row_ind]== 'numeric'){
      
      coefficient_ind <- which(q_id == coefs$question)

      coefficient_val <- coefs$coefficient[coefficient_ind]
      fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))

      # factor levels that are ordered
    } else if (grepl('ordered', df_qa$factor[row_ind]) & data_class[q_id] == 'ordered'){

      ordered_lev <- gsub('^ordered\\*', '', df_qa$factor[row_ind])
      coefficient_ind <- which(grepl(q_id, coefs$question)==TRUE)
      
      coefficient_val <- coefs$coefficient[coefficient_ind]
      poly_deg <- contr.poly(length(levels(model$data[,q_id])))
      ord <- which(levels(model$data[,q_id])==ordered_lev)
      
      xnew <- poly_deg[ord, ]
      
      coefficient_val <- sum(xnew*coefficient_val)

      fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))
      
      # factor levels that are logical
    } else if (grepl('logical', df_qa$factor[row_ind]) & data_class[q_id] == 'logical'){
      
      logical_lev <- gsub('^logical\\*', '', df_qa$factor[row_ind])
      
      if (logical_lev == 'TRUE'){
        
        coefficient_ind <- which(grepl(paste(q_id, 'TRUE', sep=''), coefs$question)==TRUE)
        coefficient_val <- 0.5*coefs$coefficient[coefficient_ind]
        
        fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))
        
      } else if (logical_lev == 'FALSE'){
        
        coefficient_ind <- which(grepl(paste(q_id, 'TRUE', sep=''), coefs$question)==TRUE)
        coefficient_val <- -0.5*coefs$coefficient[coefficient_ind]
        
        intercept_sum <- intercept_sum - coefficient_val
        
        fixed_coefs <- rbind(fixed_coefs, data.frame(paste(df_qa[row_ind,1], df_qa[row_ind,2], sep=''), coefficient_val))
      }
      
    }


  }
  
  df <- cbind(df_qa, fixed_coefs$coefficient_val)
  colnames(df) <- c('question', 'factor', 'coefficient_val')
  
  return(list(df=df, intercept=intercept_sum))
}

get_numeric_vals <- function(df_demo, df_coefs){
  #########################################################################################################
  # df_demo: data frame with numeric answers for interim analysis.

  # returns:data frame for numeric variables.
  #########################################################################################################

  numeric_col <- c()
  numeric_val <- c()
  
  numeric_questions <- df_coefs$question[which(df_coefs$factor == 'numeric')]
  num_numeric_answered <- length(which(numeric_questions %in% names(df_demo)))

  if (num_numeric_answered > 0){
    for (question in numeric_questions){
      if (is.null(df_demo[[question]])){
        next
      } else if (is.numeric(df_demo[[question]])){
        numeric_col <- c(numeric_col, question)
        numeric_val <- c(numeric_val, df_demo[[question]])
      }
    }
    df <- data.frame(question=numeric_col, value=numeric_val, stringsAsFactors=FALSE)
  } else {
    df <- data.frame()
  }



  return(df)
}

match_numeric_df <- function(df_demo_numeric, df_coefs){
  #########################################################################################################
  # df_demo: data frame with numeric answers for interim analysis.

  # df_coefs: data frame with coefficients for each answer choice.

  # returns: matched data frame for numeric variables.
  #########################################################################################################

  df_demo_numeric$coefficient_val <- numeric(nrow(df_demo_numeric))

  for (i in seq(1, nrow(df_demo_numeric))){
    
    ind <- which(df_coefs$question == df_demo_numeric$question[i])
    df_demo_numeric$coefficient_val[i] <- df_coefs$coef[ind] * df_demo_numeric$value[i]

  }


  return(df_demo_numeric)

}

match_factor_df <- function(df_demo, df_coefs){
  #########################################################################################################
  # df_demo: data frame for interim analysis.

  # df_coefs: data frame with coefficients for each answer choice.

  # returns: matched data frame for factor variables.
  #########################################################################################################

  df_demo_factor <- as.data.frame(t(df_demo))
  df_demo_factor['question'] <- rownames(df_demo_factor)
  colnames(df_demo_factor) <- c('factor', 'question')
  rownames(df_demo_factor) <- NULL
  df_matched_factor <- suppressMessages(match_df(df_coefs, df_demo_factor))
  return(df_matched_factor)
}

match_ordered_df <- function(df_demo, df_coefs){
  #########################################################################################################
  # df_demo: data frame for interim analysis.
  
  # df_coefs: data frame with coefficients for each answer choice.
  
  # returns: matched data frame for ordered variables.
  #########################################################################################################
  
  df_demo_factor <- as.data.frame(t(df_demo))
  df_demo_factor['question'] <- rownames(df_demo_factor)
  colnames(df_demo_factor) <- c('factor', 'question')
  rownames(df_demo_factor) <- NULL
  
  ordered_qs <- df_coefs[which(grepl('^ordered\\*', df_coefs$factor)),]
  
  ordered_qs$factor <- gsub('^ordered\\*', '', ordered_qs$factor)

  df_matched_ordered <- suppressMessages(match_df(ordered_qs, df_demo_factor))
  
  return(df_matched_ordered)
}

match_logical_df <- function(df_demo, df_coefs){
  #########################################################################################################
  # df_demo: data frame for interim analysis.
  
  # df_coefs: data frame with coefficients for each answer choice.
  
  # returns: matched data frame for logical variables.
  #########################################################################################################
  df_demo_factor <- as.data.frame(t(df_demo))
  df_demo_factor['question'] <- rownames(df_demo_factor)
  colnames(df_demo_factor) <- c('factor', 'question')
  rownames(df_demo_factor) <- NULL
  
  logical_qs <- df_coefs[which(grepl('^logical\\*', df_coefs$factor)),]
  
  logical_qs$factor <- gsub('^logical\\*', '', logical_qs$factor)
  
  df_matched_logical <- suppressMessages(match_df(logical_qs, df_demo_factor))
  
  return(df_matched_logical)
  
}


calculate_normalizer <- function(df_coefs, answered_questions){
  #########################################################################################################
  # df_coefs: data frame with coefficients for each answer choice.
  
  # answered_questions: questions that have been answered in the questionnaire.
  
  # returns: value representing a fraction of the relative strength of questions answered based on coefficients.
  #########################################################################################################
  
  question_list <- c()
  percent_list <- c()
  

  for (question in unique(df_coefs$question)){
    
    df_question <- df_coefs[which(df_coefs$question==question), ]
    
    val <- max(abs(df_question$coefficient_val))
    
    percent_list <- c(percent_list, val)
    question_list <- c(question_list, question)
    
  }
  
  normalizer_df <- data.frame('question'=question_list, 'percent'=percent_list)
  
  normalizer <- sum(normalizer_df$percent[which(normalizer_df$question %in% answered_questions)])/sum(normalizer_df$percent)
  
  return(normalizer)
}


shift_coefficients <- function(df_coefs, intercept){
  #########################################################################################################
  # df_coefs: data frame with coefficients for each answer choice.
  
  # intercept: intercept of the model.
  
  # returns: new values for coefficients for questions with three answers centered around zero.
  #########################################################################################################
  
  for (question in unique(df_coefs$question)){
    
    df_question <- df_coefs[which(df_coefs$question==question), ]
    
    if (length(df_question$factor) == 3 & !(TRUE %in% grepl('^ordered\\*', df_question$factor))){
      
      sorted_coefs <- df_question$coefficient_val[order(df_question$coefficient_val, decreasing=TRUE)]
      sorted_factors <- df_question$factor[order(df_question$coefficient_val, decreasing=TRUE)]
      
      intercept <- intercept + sorted_coefs[2]
      sorted_coefs <- sorted_coefs - sorted_coefs[2]

      df_coefs$factor[which(df_coefs$question==question)] <- sorted_factors
      df_coefs$coefficient_val[which(df_coefs$question==question)] <- sorted_coefs

    }
    
  }   
  
  return(list(df_coefs=df_coefs, intercept=intercept))
}



# function to calculate interim analysis probability and label
ia_probability <- function(model, df_matched_factor, df_matched_numeric, df_matched_ordered, df_matched_logical, df_coefs,
                           df_max_numeric, zero_label, one_label, intercept, damping_factor,
                           answered_questions){

  #########################################################################################################
  # model: model to be used in IA.

  # df_matched_factor: data frame with matched factors.

  # df_matched_numeric: data frame with matched numeric questions.

  # df_coefs: data frame with coefficients for each answer choice.

  # df_max_numeric: ??????

  # zero_label: Label corresponding to <0.5 probability.

  # one_label: Label corresponding to >0.5 probability.

  # intercept: The intercept of the model.

  # returns: the IA probability.
  #########################################################################################################

  num_answered <- nrow(df_matched_factor) + nrow(df_matched_numeric) + nrow(df_matched_ordered) + nrow(df_matched_logical)
  num_questions <- length(attributes(model$terms)$term.labels)
  var_sum <- sum(df_matched_factor$coefficient_val) + 
    sum(df_matched_numeric$coefficient_val) +
    sum(df_matched_ordered$coefficient_val) +
    sum(df_matched_logical$coefficient_val) +
    intercept

  normalizer <- calculate_normalizer(df_coefs, answered_questions)
  
  # pred_prob <- model$family$linkinv(var_sum*((num_answered/num_questions)^damping_factor))
  pred_prob <- model$family$linkinv(var_sum*normalizer^damping_factor)
  
  if (pred_prob >= 0.5){
    pred_label <- one_label
  } else if (pred_prob < 0.5){
    pred_label <- zero_label
  }

  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))

}


# Function to generate a generic interim predictor function
create_interim_predictor <- function(model, zero_lab, one_lab, df_max_numeric, damping_factor){

  #########################################################################################################
  # model: model to be used in IA.

  # zero_label: Label corresponding to <0.5 probability.

  # one_label: Label corresponding to >0.5 probability.

  # df_max_numeric: data frame that contains the max values allowed for each numeric question

  # returns: the IA prediction function for the model.
  #########################################################################################################

  qa_combinations <- create_qa_combinations(model)
  
  df_test <- fill_coefs(qa_combinations, model)
  intercept <- df_test$intercept
  df_coefs <- df_test$df
  
  df_coef_transform <- shift_coefficients(df_coefs, intercept)
  df_coefs <- df_coef_transform$df_coefs
  intercept <- df_coef_transform$intercept

  interim_predictor <- function(df) {
    if (nrow(df) == 0) {
      return(list(
        prediction=zero_lab,
        probability = 0.5
      ))
    }
    answered_questions <- names(df)
    df_demo_numeric <- get_numeric_vals(df, df_coefs)
    df_matched_numeric <- match_numeric_df(df_demo_numeric, df_coefs)

    df_matched_factor <- match_factor_df(df, df_coefs)
    
    df_matched_logical <- match_logical_df(df, df_coefs)
    
    df_matched_ordered <- match_ordered_df(df, df_coefs)

    pred <- ia_probability(model, df_matched_factor, df_matched_numeric, df_matched_ordered, df_matched_logical,
                           df_coefs, df_max_numeric, zero_lab, one_lab, intercept, damping_factor, answered_questions)

    return(pred)
  }

  return(interim_predictor)
}
