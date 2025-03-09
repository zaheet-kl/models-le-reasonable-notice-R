ten_thirty_one_calculator_factors <- function() {
  
  return(c(
    "1.1",
    "1.2",
    "1.3",
    "1.4",
    "1.4.1",
    "1.5",
    "1.5.1",
    "1.5.2",
    "2.1",
    "2.1.1",
    "2.1.2",
    "2.2",
    "2.2.1",
    "2.3",
    "2.4",
    "2.5"
  ))
}

ten_thirty_one_calculator_predict <- function(df) {
  
  df <- format_dataframe_numerics(df, c("X1.2", "X1.3", "X1.4.1", "X1.5",
                                        "X1.5.1", "X1.5.2", "X2.1.1","X2.1.2",
                                        "X2.2", "X2.2.1", "X2.3", "X2.4", "X2.5"))
  
  ten_thirty_one <- data.frame(result = '',
                               RGE = '',
                               MNG = '',
                               RCG = '',
                               DG = '',
                               DL = '',
                               BRP = '',
                               RGOP = '',
                               RLOP = '')
  
  if (df$X1.1=='Yes' & df$X1.4=='No'){
    
    ten_thirty_one$result <- 'Qualifying Property Only, Adjusted Basis Known'
    ten_thirty_one$RCG <- 0
    ten_thirty_one$DG <- max(0, df$X1.2 - df$X1.4.1)
    ten_thirty_one$DL <- max(0, df$X1.4.1 - df$X1.2)
    ten_thirty_one$BRP <- df$X1.4.1
    
  } else if (df$X1.1=='Yes' & df$X1.4=='Yes'){    
    
    ten_thirty_one$result <- 'Qualifying Property Only, Adjusted Basis Unknown'  
    ten_thirty_one$RCG <- 0
    ten_thirty_one$DG <- max(0, df$X1.2 - (df$X1.5 + df$X1.5.1 - df$X1.5.2))
    ten_thirty_one$DL <- max(0, (df$X1.5 + df$X1.5.1 - df$X1.5.2) - df$X1.2)
    ten_thirty_one$BRP <- (df$X1.5 + df$X1.5.1 - df$X1.5.2)
    
  } else if (df$X1.4== 'No' & df$X2.1=='Cash only'){    
    
    ten_thirty_one$result <- 'Cash Boot Only, Adjusted Basis Known'  
    ten_thirty_one$RGE <- max(0, (df$X1.2 - df$X1.4.1 + df$X2.1.2))
    ten_thirty_one$RCG <- min(df_out$RGE, df$X2.1.2)
    ten_thirty_one$DG <- max(0, df_out$RGE - df_out$RCG)
    ten_thirty_one$DL <- max(0, df$X1.4.1 - df$X1.2)
    ten_thirty_one$BRP <- df$X1.4.1 + df$X2.1.1 - df$X2.1.2 + df_out$RCG 
    
  } else if (df$X1.4== 'Yes' & df$X2.1=='Cash only'){   
    
    ten_thirty_one$result <- 'Cash Boot Only, Adjusted Basis Unknown'
    ten_thirty_one$RGE <- max(0, (df$X1.2 - (df$X1.5 + df$X1.5.1 - df$X1.5.2) + df$X2.1.2))
    ten_thirty_one$RCG <- min(df_out$RGE, df$X2.1.2)
    ten_thirty_one$DG <- max(0, df_out$RGE - df_out$RCG)
    ten_thirty_one$DL <- max(0, (df$X1.5 + df$X1.5.1 - df$X1.5.2) - df$X1.2)
    ten_thirty_one$BRP <- (df$X1.5 + df$X1.5.1 - df$X1.5.2) + df$X2.1.1 - df$X2.1.2 + df_out$RCG 
    
  } else if (df$X1.4== 'No' & df$X2.1=='Cash equivalents, debt relief, or any non-qualifying or non-like-kind property, in addition to or in place of cash'){
    
    ten_thirty_one$result <- 'Other Boot, Adjusted Basis Known' 
    ten_thirty_one$RGE <- max(0, (df$X1.2 - df$X1.4.1 + df$X2.1.2 + df$X2.3 + df$X2.4 - df$X2.5))
    ten_thirty_one$MNG <- max(0, df$X2.4 - df$X2.5)
    ten_thirty_one$RCG <- min(df_out$RGE, df$X2.1.2 + df$X2.3 + df_out$MNG)
    ten_thirty_one$DG <- max(0, df_out$RGE - df_out$RCG)
    ten_thirty_one$DL <- max(0, df$X1.4.1 - df$X1.2)
    ten_thirty_one$BRP <- df$X1.4.1 + (df$X2.1.1 + df$X2.2 + df$X2.5) - (df$X2.1.2 + df$X2.3) - df$X2.4 + df_out$RCG
    ten_thirty_one$RGOP <- max(0, df$X2.2 - df$X2.2.1)
    ten_thirty_one$RLOP <- max(0, df$X2.2.1 - df$X2.2)
    
    
  } else if (df$X1.4== 'Yes' & df$X2.1=='Cash equivalents, debt relief, or any non-qualifying or non-like-kind property, in addition to or in place of cash'){
    
    ten_thirty_one$result <- 'Other Boot, Adjusted Basis Unknown' 
    ten_thirty_one$RGE <- max(0, (df$X1.2 - (df$X1.5 + df$X1.5.1 - df$X1.5.2) + df$X2.1.2 + df$X2.3 + df$X2.4 - df$X2.5))
    ten_thirty_one$MNG <- max(0, df$X2.4 - df$X2.5)
    ten_thirty_one$RCG <- min(df_out$RGE, df$X2.1.2 + df$X2.3 + df_out$MNG)
    ten_thirty_one$DG <- max(0, df_out$RGE - df_out$RCG)
    ten_thirty_one$DL <- max(0, (df$X1.5 + df$X1.5.1 - df$X1.5.2) - df$X1.2)
    ten_thirty_one$BRP <- (df$X1.5 + df$X1.5.1 - df$X1.5.2) + (df$X2.1.1 + df$X2.2 + df$X2.5) - (df$X2.1.2 + df$X2.3) - df$X2.4 + df_out$RCG
    ten_thirty_one$RGOP <- max(0, df$X2.2 - df$X2.2.1)
    ten_thirty_one$RLOP <- max(0, df$X2.2.1 - df$X2.2)
    
  } else {    
    
    ten_thirty_one$result <- 'Insufficient Information to Compute'  
    
  }

  return(list(
    prediction=ten_thirty_one$result,
    subResults=list(
      list(
        group='BRP',
        prediction=ten_thirty_one$BRP
      ),
      list(
        group='DG',
        prediction=ten_thirty_one$DG
      ),
      list(
        group='DL',
        prediction=ten_thirty_one$DL
      ),
      list(
        group='MNG',
        prediction=ten_thirty_one$MNG
      ),
      list(
        group='RCG',
        prediction=ten_thirty_one$RCG
      ),
      list(
        group='RGE',
        prediction=ten_thirty_one$RGE
      ),
      list(
        group='RGOP',
        prediction=ten_thirty_one$RGOP
      ),
      list(
        group='RLOP',
        prediction=ten_thirty_one$RLOP
      )
    )
  ))
  
}
