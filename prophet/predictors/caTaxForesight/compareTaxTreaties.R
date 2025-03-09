compare_tax_treaties_factors <- function(){
  labels <- c('1.1')
  return(labels)
}

compare_tax_treaties_predict <- function(df){
  
  if (!is.na(df$X1.1)){
    return(list(prediction='Compare treaty withholding rates'))
  }
  
  return(list(prediction='No comparison'))
}
