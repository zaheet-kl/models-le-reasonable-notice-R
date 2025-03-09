YES <- 'Yes'
NOPE <- 'No'
NONRESIDENT <- 'Nonresident'
RESIDENT <- 'Resident'
CLOSER <- 'Closer connection analysis required'
DUAL <- 'Dual resident (Tiebreaker analysis required)'
UNABLE <- 'Unable to compute result'

us_residency_factors <- function(){
  labels <- c(
    '1.0_customChoiceValue',
    '1.2',
    '2.1.1',
    '2.2',
    '3.1',
    '3.3',
    '3.4',
    '3.5',
    '4.3'
    )
  return(labels)
}

us_residency_predict <- function(df){
  
  df <- format_dataframe_numerics(df, c('X3.3', 'X3.4', 'X3.5'))
  
  SPT <- df$X3.3 + (df$X3.4)/3 + (df$X3.5)/6 
  
  if(df$X1.0_customChoiceValue == 'Other'){
    pred_label <- UNABLE
  } else if(df$X2.1.1 == NOPE |
            df$X3.1 == NOPE |
            df$X3.3 < 31 |
            SPT < 183 |
            df$X4.3 == YES){
    pred_label <-  NONRESIDENT
  } else if(df$X4.3 == 'Unsure'){
    pred_label <- CLOSER
  } else if(df$X2.2 == YES |
            (df$X1.2 == NOPE &
            (df$X2.1.1 == YES |
             df$X3.3 >= 183 |
             SPT >= 183 |
             df$X4.3 == NOPE))){
    pred_label <- RESIDENT
  } else if(df$X1.2 == YES &
            (df$X2.1.1 == YES |
             df$X3.3 >= 183 |
             SPT >= 183 |
             df$X4.3 == NOPE)){
    pred_label <- DUAL
  } else{
    pred_label <- NONRESIDENT
  }
  
  return(list(
    prediction=pred_label
  ))
}
