foreign_affiliate_controlled_foriegn_affiliate_factors <- function(){
  return(c(
    '1.3',
    '1.5.13',
    '1.5.22',
    '1.5',
    '1.5.12m',
    '1.5.12s',
    '1.5.21',
    '1.6',
    '2.2',
    '2.4',
    '2.7',
    '3.2.12',
    '3.2.40',
    '3.2.32',
    '3.3.22',
    '3.3.32',
    '3.3.40',
    '3.1',
    '3.1.1',
    '3.2',
    '3.2.11',
    '3.2.21',
    '3.2.22',
    '3.2.31',
    '3.3',
    '3.3.11',
    '3.3.12',
    '3.3.21',
    '3.3.31',
    '3.3.51'
  ))
}



foreign_affiliate_controlled_foriegn_affiliate_predict <- function(df){
  
  # change default numeric values from strings to numeric
  df[which(df=="0")] <- as.numeric("0")
  
  result <- ''
  
  if (df$X1.5 == '10% or greater' |
      df$X1.5.12m == '10% or greater' |
      df$X1.5.12s == '10% or greater' |
      df$X1.5.13 >= 10 |
      df$X1.5.21 == '10% or greater' |
      df$X1.5.22 >= 10 |
      df$X1.6 == '10% or greater' |
      df$X2.2 == 'Yes' |
      df$X2.4 == 'Yes' |
      df$X2.7 == 'Yes'){
    
    result <- 'Foreign Affiliate'
  }
  
  nal_percent <- df$X3.2.12
  taxpayer_voting_percent <- df$X3.2.40
  relevant_sh_nal_percent <- df$X3.2.32
  relevant_sh_percent <- df$X3.2.22
  resident_nal_percent <- df$X3.3.12
  resident_relevant_sh_percent <- df$X3.3.22
  resident_relevant_sh_nal_percent <- df$X3.3.32
  resident_taxpayer_voting_percent <- df$X3.3.40
  
  control_percent <- taxpayer_voting_percent + relevant_sh_nal_percent +
    relevant_sh_percent + nal_percent
  
  resident_control_percent <- resident_taxpayer_voting_percent + resident_relevant_sh_nal_percent +
    resident_relevant_sh_percent + resident_nal_percent
  
  if (result == 'Foreign Affiliate' &
      (df$X3.1 == 'Yes' |
       df$X3.1.1 == 'Yes' |
       df$X3.2 == 'Yes' |
       df$X3.2.11 == 'Yes' |
       df$X3.2.12 > 50 |
       df$X3.2.21 == 'Yes' |
       df$X3.2.22 > 50 |
       df$X3.2.31 == 'Yes' |
       df$X3.2.32 > 50 |
       df$X3.2.40 > 50 |
       control_percent > 50 |
       df$X3.3 == 'Yes' | 
       df$X3.3.11 == 'Yes' |
       df$X3.3.12 > 50 |
       df$X3.3.21 == 'Yes' |
       df$X3.3.22 > 50 |
       df$X3.3.31 == 'Yes' |
       df$X3.3.32 > 50 |
       df$X3.3.40 > 50 |
       resident_control_percent > 50 |
       df$X3.3.51 == 'Yes')){
    
    result <- 'Controlled Foreign Affiliate'
  }
  
  if (result == '' |
      df$X1.3 == 'No'){
    result <- 'Not a Foreign Affiliate'
  }
  
  return(list(
    prediction=result,
    subResults=list(
      list(
        group='nal_percent',
        prediction=nal_percent
        ),
      list(
        group='taxpayer_voting_percent',
        prediction=taxpayer_voting_percent
        ),
      list(
        group='relevant_sh_nal_percent',
        prediction=relevant_sh_nal_percent
        ),
      list(
        group='relevant_sh_percent',
        prediction=relevant_sh_percent
        ),
      list(
        group='resident_nal_percent',
        prediction=resident_nal_percent
        ),
      list(
        group='resident_relevant_sh_percent',
        prediction=resident_relevant_sh_percent
        ),
      list(
        group='resident_relevant_sh_nal_percent',
        prediction=resident_relevant_sh_nal_percent
      ),
      list(
        group='resident_taxpayer_voting_percent',
        prediction=resident_taxpayer_voting_percent
      ),
      list(
        group='control_percent',
        prediction=control_percent
      ),
      list(
        group='resident_control_percent',
        prediction=resident_control_percent
        )
    )
  ))
  
  
}
