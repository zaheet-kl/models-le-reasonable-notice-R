covid_us_factors <- function(){
  return(c(
    '1.1',	
    '2.0',	
    '2.1',
    '2.1.2',
    '2.6.1',
    '2.6.2',
    '2.6.4',
    '2.6.4.1',
    '2.7.1',
    '2.7.1.1',
    '2.7.1.2',
    '2.7.1.3',
    '3.2.1',
    '3.2.2',
    '3.2.3',
    '3.2.4',
    '3.3.7',
    '3.3.8',
    '3.3.9',
    '3.3.10',
    '3.4.4',	
    '3.4.5',
    '3.4.6',
    '3.4.4.1',
    '3.7.4',
    '3.7.1',
    '3.7.1.1',
    '3.7.2',	
    '3.7.2.1',	
    '3.7.3',	
    '3.7.3.1',	
    '4.3',	
    '4.3.1',
    '4.6',	
    '4.6.1',	
    '4.6.2',	
    '5.3',	
    '5.3.2',	
    '5.3.1',	
    '5.3.3',
    '5.3.4'
  ))
}



covid_us_predict <- function(df){
  
  df <- format_dataframe_numerics(df, c('X2.1.2',
                                        'X2.6.4',
                                        'X2.6.4.1',
                                        'X2.7.1.1',
                                        'X2.7.1.2',
                                        'X2.7.1.3',
                                        'X3.2.2',
                                        'X3.2.3',
                                        'X3.2.4',
                                        'X3.3.8',
                                        'X3.3.9',
                                        'X3.3.10',
                                        'X3.4.5',
                                        'X3.4.6',
                                        'X3.4.4.1',
                                        'X3.7.4',
                                        'X3.7.1.1',
                                        'X3.7.2.1',
                                        'X3.7.3.1',
                                        'X4.3.1',
                                        'X4.6.1',
                                        'X4.6.2',
                                        'X5.3.2',
                                        'X5.3.3',
                                        'X5.3.4'
                                        ))
  
  df[which(is.na(df)==T)] <- 'default'
  
  covid_us <- data.frame(result='',
                         recovery_rebate='',
                         early_withdrawal='',
                         loan_taken='',
                         nol_carryable='',
                         ppp_individual='',
                         ppp_autoforgive='',
                         ppp_business='',
                         erc_business='',
                         sick_leave_high='',
                         sick_leave_low='',
                         family_leave='',
                         amt_refund='',
                         chari_do='',
                         qdr_payment='',
                         wage_wage='')
  
  
  if (df$X1.1 == 'An individual who is a U.S. citizen or considered as a U.S. resident for the federal income tax purposes in the tax year of 2020' |
      df$X1.1== 'An individual who is considered as a nonresident alien for U.S. federal income tax purposes in the tax year of 2020'){
    covid_us$result <- 'Tax Reliefs for Individual'      
  } else if (df$X2.0 == 'A domestic C-corporation that engages in a trade or business in the U.S.' |
             df$X2.0 == 'A foreign corporation taxable for U.S. federal income tax purposes for effectively connected income in 2014, 2015, 2016, 2017, 2018, 2019 or 2020' |
             df$X2.0 == 'A pass-through business entity that engages in a trade or business in the U.S.'){
    covid_us$result <- 'Tax Reliefs for Corporation or Pass-Through Entity'             
  } else {
    covid_us$result <- 'Reliefs Outside of this Analysis'
  }
  
  
  if (df$X2.1 == 'Yes'){
    covid_us$recovery_rebate = df$X2.1.2
  }
  
  
  
  
  if (df$X2.6.1=='Yes' | df$X2.6.2=='Yes'){
    covid_us$early_withdrawal = min(100000, df$X2.6.4)
    covid_us$loan_taken = min(100000, df$X2.6.4.1)
  }
  
  
  if (df$X2.7.1=='Would have generated or otherwise incurred NOL in the tax year of 2018'){
    covid_us$nol_carryable = df$X2.7.1.1
  } else if (df$X2.7.1=='Would have generated or otherwise incurred NOL in the tax year of 2019'){
    covid_us$nol_carryable = df$X2.7.1.2
  } else if (df$X2.7.1=='Both of the above'){
    covid_us$nol_carryable = df$X2.7.1.3
  }
  
  
  
  if (df$X3.2.1=='Yes'){
    covid_us$ppp_individual = min((20833 + df$X3.2.3 - df$X3.2.4), (df$X3.2.2*2.5 + df$X3.2.3 - df$X3.2.4))
    covid_us$ppp_autoforgive = min(12450, df$X3.2.2*2.5*0.6)
  }
  
  
  if (df$X3.3.7=='Yes'){
    covid_us$ppp_business = min(10000000, (df$X3.3.8*2.5) + df$X3.3.9 - df$X3.3.10)
  }
  
  
  if (df$X3.4.4=='Yes' & df$X3.4.5 > 0){
    covid_us$erc_business = df$X3.4.6*0.5
  } 
  
  if (df$X3.4.4=='No'){
    covid_us$erc_business = df$X3.4.4.1*0.5
  }
  
  
  if (df$X3.7.1=='Yes' & df$X3.7.4 > 0){
    covid_us$sick_leave_high = min(df$X3.7.4*df$X3.7.1.1, 511*df$X3.7.1.1, 5110)    
  }
  
  
  if (df$X3.7.2=='Yes' & df$X3.7.4 > 0){
    covid_us$sick_leave_low = min(df$X3.7.4*df$X3.7.2.1*0.67, 200*df$X3.7.2.1, 2000)
  }
  
  
  if (df$X3.7.3=='Yes' & df$X3.7.4 > 0){
    covid_us$family_leave = min(df$X3.7.4*df$X3.7.3.1*0.67, 200*df$X3.7.3.1, 100000)
  }
  
  
  if (df$X4.3=='Yes' & df$X2.0=='A domestic C-corporation that engages in a trade or business in the U.S.'){
    covid_us$amt_refund = df$X4.3.1
  }
  
  
  if (df$X4.3=='Yes' & df$X2.0=='A foreign corporation taxable for U.S. federal income tax purposes for effectively connected income in 2014, 2015, 2016, 2017, 2018, 2019 or 2020'){
    covid_us$amt_refund = df$X4.3.1
  }
  
  
  if (df$X4.6=='Donation of its apparently wholesome food inventory to a charitable organization solely for the care of ill, the needy or infants'){
    covid_us$chari_do = min(df$X4.6.2*2, df$X4.6.2 + (df$X4.6.1 - df$X4.6.2)*0.5)
  }
  
  
  
  if (df$X5.3 == 'Yes'){
    covid_us$qdr_payment = df$X5.3.2
  }
  
  
  if (df$X5.3.1=='Yes'){
    covid_us$wage_wage = df$X5.3.3 + df$X5.3.4
  }
  
  
  
  return(list(
    prediction=covid_us$result,
    subResults=list(
      list(
        group='recovery_rebate',
        prediction=covid_us$recovery_rebate
      ),
      list(
        group='early_withdrawal',
        prediction=covid_us$early_withdrawal
      ),
      list(
        group='loan_taken',
        prediction=covid_us$loan_taken
      ),
      list(
        group='nol_carryable',
        prediction=covid_us$nol_carryable
      ),
      list(
        group='ppp_individual',
        prediction=covid_us$ppp_individual
      ),
      list(
        group='ppp_autoforgive',
        prediction=covid_us$ppp_autoforgive
      ),
      list(
        group='ppp_business',
        prediction=covid_us$ppp_business
      ),
      list(
        group='erc_business',
        prediction=covid_us$erc_business
      ),
      list(
        group='sick_leave_high',
        prediction=covid_us$sick_leave_high
      ),
      list(
        group='sick_leave_low',
        prediction=covid_us$sick_leave_low
      ),
      list(
        group='family_leave',
        prediction=covid_us$family_leave
      ),
      list(
        group='amt_refund',
        prediction=covid_us$amt_refund
      ),
      list(
        group='chari_do',
        prediction=covid_us$chari_do
      ),
      list(
        group='qdr_payment',
        prediction=covid_us$qdr_payment
      ),
      list(
        group='wage_wage',
        prediction=covid_us$wage_wage
      )
    )
  ))
  
  
}

