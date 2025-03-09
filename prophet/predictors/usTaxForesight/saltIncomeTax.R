salt_income_tax_constants <- data.frame(COMPARE = 'compare_state_laws',
                                     NO_COMPARISON = 'No comparison',
                                     stringsAsFactors=FALSE)

salt_income_tax_factors <- function(){
  labels <- c('1.1')
  return(labels)
}

salt_income_tax_predict <- function(df){
  
  if (!is.na(df$X1.1)){
    return(list(prediction=salt_income_tax_constants$COMPARE))
  }
  
  return(list(prediction=salt_income_tax_constants$NO_COMPARISON))
}
