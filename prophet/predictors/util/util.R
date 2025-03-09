format_dataframe_numerics <- function(df, continuous_factors) {
  return(cbind(
    df[, -which(names(df) %in% continuous_factors)],
    lapply(df[continuous_factors],
           function(f) {
             if (is.null(levels(f))) {
               return(f)
             }
             return(as.numeric(levels(f))[f])
           }
    )
  ))
}

convert_probability_above_50 <- function(prob){
  if (prob < 0.5){
    return(1-prob)
  } else {
    return(prob)
  }
}


empty_question_transformation <- function(df, factor_function, remove, answered_questions){
  
  all_questions <- factor_function()
  all_questions <- paste("X", all_questions, sep="")
  
  # get the null questions
  null_questions <- all_questions[which(!(all_questions %in% answered_questions))]
  
  if (!remove){
    if (nrow(df)==0){
      df <- as.data.frame.list(rep('UNANSWERED', length(all_questions)))
      names(df) <- all_questions
    } else{
      df[null_questions] <- list(rep(factor('UNANSWERED'), length(null_questions)))
    }
  } else if (remove){
    df[null_questions] <- list(rep(NULL, length(null_questions)))
  } else {
    stop('Specify remove (TRUE/FALSE)')
  }

  return(df)
}

  

  
  