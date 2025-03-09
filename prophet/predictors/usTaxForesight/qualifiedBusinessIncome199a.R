qbi_199a_factors <- function(){
  return(c(
    "1.1",
    "1.2",
    "1.2.1",
    "1.3",
    "1.4a",
    "1.4b",
    "1.4c",
    "1.4c.1",
    "1.5",
    "2.1_1",
    "2.1_2",
    "2.2",
    "2.2.1",
    "2.3",
    "2.4",
    "2.5",
    "2.6",
    "2.6.1",
    "2.6.2",
    "2.6.3",
    "2.7",
    "2.8",
    "2.9",
    "3.1a",
    "3.1b.1",
    "3.1b.2",
    "3.1c.1",
    "3.1c.2",
    "3.1d.1",
    "3.1d.2",
    "3.2a",
    "3.2b.1",
    "3.2b.2",
    "3.2c.1",
    "3.2c.2",
    "3.2d.1",
    "3.2d.2",
    "2.10",
    "2.11",
    "2.12",
    "3.1e.1",
    "3.1e.2",
    "3.2e.1",
    "3.2e.2"
  ))
}


qbi_199a_predict <- function(df){
  
  df <- format_dataframe_numerics(df, c('X1.1', 'X1.4a', 'X1.4b', 'X1.4c', 'X1.4c.1', 'X1.5',
                                        'X2.2', 'X2.2.1', 'X2.3', 'X2.4', 'X2.5', 'X2.6.1',
                                        'X2.6.2', 'X2.6.3', 'X2.7', 'X2.8', 'X2.9', 'X3.1a',
                                        'X3.1b.1', 'X3.1b.2', 'X3.1c.1', 'X3.1c.2', 'X3.1d.1',
                                        'X3.1d.2', 'X3.2a', 'X3.2b.1', 'X3.2b.2', 'X3.2c.1',
                                        'X3.2c.2', 'X3.2d.1', 'X3.2d.2', 'X2.10', 'X2.11',
                                        'X2.12', 'X3.1e.1', 'X3.1e.2', 'X3.2e.1', 'X3.2e.2'))
  
  qbi_199a <- data.frame(result = '',
                         dpad = '',
                         qbd = '',
                         ti = '',
                         lit = '',
                         qii = '',
                         qbiqp = '',
                         tdqbi = '',
                         ceil1 = '',
                         ceil2 = '',
                         cap = '',
                         exp_ = '',
                         fs = '',
                         clb = '',
                         shortcut1 = '',
                         rd = '',
                         ap = '',
                         ceil1a = '',
                         ceil1b = '',
                         ceil1c = '',
                         ceil2a = '',
                         ceil2b = '',
                         ceil2c = '',
                         cap1 = '',
                         clb1 = '',
                         cap2 = '',
                         clb2 = '',
                         cap3 = '',
                         clb3 = '',
                         tdqbi1 = '',
                         tdqbi2 = '',
                         tdqbix = '',
                         ceil1x = '',
                         ceil2x = '',
                         capx = '',
                         clbx = '',
                         tdqbiy = '',
                         ceil1y = '',
                         ceil2y = '',
                         capy = '',
                         clby = '',
                         dqbi = '',
                         cqbi = '',
                         qbid = '',
                         shortcut2 = '',
                         texp = '')
  
  df[which(is.na(df)==T)] <- 'default'
  
  qbi_199a$dpad <- df$X2.6.2
  
  qbi_199a$qbd <- max(0, min(df$X1.4c.1*0.5, df$X1.4c*0.09))
  
  qbi_199a$ti <- df$X1.4a
  
  qbi_199a$lit <- max(0, (qbi_199a$ti - df$X1.5)*0.2)
  
  qbi_199a$qii <- max(0, df$X2.2 - df$X2.2.1 + df$X2.3 + df$X2.4 - df$X2.5)
  
  qbi_199a$qbiqp <- df$X2.6.1*0.2
  
  if (df$X2.1_1=='Income or loss from a qualified trade or business (QTB)'){
    qbi_199a$tdqbi <- max(0, df$X2.7*0.2)
  } else if (df$X2.1_1=='Income or loss from a specified service trade or business (SSTB)'){
    qbi_199a$tdqbi <- max(0, df$X2.8*0.2)
  } else if (df$X2.1_1=='Income or loss from multiple qualified trades or businesses (QTBs) eligible to be aggregated as a single trade or business'){
    qbi_199a$tdqbi <- max(0, df$X2.9*0.2)
  } else {
    qbi_199a$tdqbi <- 0
  }

  
  qbi_199a$ceil1 <- df$X3.1a*0.5
  
  qbi_199a$ceil2 <- df$X3.1a*0.25 + df$X3.2a*0.025
  
  qbi_199a$cap <- max(qbi_199a$ceil1, qbi_199a$ceil2)
  
  # 2018
  if (df$X1.1==2018){
    qbi_199a$exp_ <- 157500
    # 2019
  } else if (df$X1.1==2019){ 
    if (df$X1.3=='Married Filing Separately'){
      qbi_199a$exp_ <- 160725
    } else {
      qbi_199a$exp_ <- 160700
    } 
    # 2020 
  } else if (df$X1.1==2020){
    qbi_199a$exp_ <- 163300
    # 2021
  } else if (df$X1.1==2021){
    if (df$X1.3=='Married Filing Separately'){
      qbi_199a$exp_ <- 164925
    } else {
      qbi_199a$exp_ <- 164900
    }
  } else{
    qbi_199a$exp_ <- 0
  }
  
  if (df$X1.2.1=='No' |
      df$X1.3=='Single or Head of the Household' |
      df$X1.3=='Married Filing Separately'){
    qbi_199a$fs <- 1
  } else if (df$X1.3=='Married Filing Jointly'){
    qbi_199a$fs <- 2
  } else {
    qbi_199a$fs <- 0
  }
  
  if (qbi_199a$fs==0){
    qbi_199a$clb <- 0
  } else (
    qbi_199a$clb <- max(0, min((qbi_199a$tdqbi - qbi_199a$cap), (qbi_199a$tdqbi - qbi_199a$cap)*((qbi_199a$ti - qbi_199a$exp_*qbi_199a$fs)/(50000*qbi_199a$fs))))
  )
  
  if (qbi_199a$exp_*qbi_199a$fs < qbi_199a$ti){
    qbi_199a$shortcut1 <- TRUE
  } else {
    qbi_199a$shortcut1 <- FALSE
  }
  
  if (df$X2.1_1=='Qualified payment from a specified cooperative' &
      qbi_199a$shortcut1 == FALSE){
    qbi_199a$rd <- min(df$X2.6.1*0.2*0.09, df$X2.6.3*0.5)
  } else if (df$X2.1_1=='Qualified payment from a specified cooperative' &
             qbi_199a$shortcut1 == TRUE){
    qbi_199a$rd <- min(df$X2.6.1*0.2*0.09, df$X3.1a*0.5)
  } else if (df$X2.1_2=='The multiple trades or businesses involves a qualified trade or business (QTB) and qualified payment from a specified cooperative' &
             qbi_199a$shortcut1 == FALSE){
    qbi_199a$rd <- min(df$X2.6.1*0.2*0.09, df$X2.6.3*0.5)
  } else if (df$X2.1_2=='The multiple trades or businesses involves a qualified trade or business (QTB) and qualified payment from a specified cooperative' &
             qbi_199a$shortcut1 == TRUE){
    qbi_199a$rd <- min(df$X2.6.1*0.2*0.09, df$X3.1b.1*0.5)
  } else if (df$X2.1_2=='The multiple trades or businesses involve qualified payment from a specified cooperative as well as income, gain, deduction and loss from a specified service trade or business operate directly by the specified cooperative' &
             qbi_199a$shortcut1 == FALSE){
    qbi_199a$rd <- min(df$X2.6.1*0.2*0.09, df$X2.6.3*0.5)
  } else if (df$X2.1_2=='The multiple trades or businesses involve qualified payment from a specified cooperative as well as income, gain, deduction and loss from a specified service trade or business operate directly by the specified cooperative' &
             qbi_199a$shortcut1 == TRUE){
    qbi_199a$rd <- min(df$X2.6.1*0.2*0.09, df$X3.1d.1*0.5)
  } else {
    qbi_199a$rd <- 0
  }
  
  
  if (qbi_199a$fs==0){
    qbi_199a$ap <- 0
  } else{
    qbi_199a$ap <- max(0, min(1, 1 - ((qbi_199a$ti - qbi_199a$exp_*qbi_199a$fs)/(50000*qbi_199a$fs))))
  }
  
  
  if (df$X2.1_2=='The multiple trades or businesses involves a qualified trade or business (QTB) and qualified payment from a specified cooperative'){
    qbi_199a$ceil1a <- df$X3.1b.1*0.5
    qbi_199a$ceil1b <- df$X3.1b.2*0.5
    qbi_199a$ceil1c <- 0
  } else if (df$X2.1_2=='The multiple trades or business include a qualified trade or business (QTB) and a specified service trade or business (SSTB), the two of which can be separated from one another (i.e., by separate invoice etc.)'){
    qbi_199a$ceil1b <- df$X3.1c.1*0.5
    qbi_199a$ceil1c <- df$X3.1c.2*0.5
    qbi_199a$ceil1a <- 0
  } else if (df$X2.1_2=='The multiple trades or businesses involve qualified payment from a specified cooperative as well as income, gain, deduction and loss from a specified service trade or business operate directly by the specified cooperative'){
    qbi_199a$ceil1a <- df$X3.1d.1*0.5
    qbi_199a$ceil1c <- df$X3.1d.2*0.5
    qbi_199a$ceil1b <-0
  } else {
    qbi_199a$ceil1a <- 0
    qbi_199a$ceil1b <- 0
    qbi_199a$ceil1c <- 0
  }
  
  
  if (df$X2.1_2=='The multiple trades or businesses involves a qualified trade or business (QTB) and qualified payment from a specified cooperative'){
    qbi_199a$ceil2a <- df$X3.1b.1*0.25 + df$X3.2b.1*0.025
    qbi_199a$ceil2b <- df$X3.1b.2*0.25 + df$X3.2b.2*0.025
    qbi_199a$ceil2c <- 0
  } else if (df$X2.1_2=='The multiple trades or business include a qualified trade or business (QTB) and a specified service trade or business (SSTB), the two of which can be separated from one another (i.e., by separate invoice etc.)'){
    qbi_199a$ceil2b <- df$X3.1c.1*0.25 + df$X3.2c.1*0.025
    qbi_199a$ceil2c <- df$X3.1c.2*0.25 + df$X3.2c.2*0.025
    qbi_199a$ceil2a <- 0
  } else if (df$X2.1_2=='The multiple trades or businesses involve qualified payment from a specified cooperative as well as income, gain, deduction and loss from a specified service trade or business operate directly by the specified cooperative'){
    qbi_199a$ceil2a <- df$X3.1d.1*0.25 + df$X3.2d.1
    qbi_199a$ceil2b <- df$X3.1d.2*0.25 + df$X3.2d.2
    qbi_199a$ceil2c <- 0
  } else {
    qbi_199a$ceil2a <- 0
    qbi_199a$ceil2b <- 0
    qbi_199a$ceil2c <- 0
  }
  
  qbi_199a$cap1 <- max(qbi_199a$ceil1a, qbi_199a$ceil2a)
  
  if (qbi_199a$fs==0){
    qbi_199a$clb1 <- 0
  } else{
    qbi_199a$clb1 <- max(0, min((qbi_199a$qbiqp - qbi_199a$cap1), (qbi_199a$qbiqp - qbi_199a$cap1)*((qbi_199a$ti - qbi_199a$exp_*qbi_199a$fs)/(50000*qbi_199a$fs))))
  }
  
  qbi_199a$tdqbi1 <- max(0, df$X2.7*0.2)
  
  qbi_199a$cap2 <- max(qbi_199a$ceil1b, qbi_199a$ceil2b)
  
  if (qbi_199a$fs==0){
    qbi_199a$clb2 <- 0
  } else{
    qbi_199a$clb2 <- max(0, min((qbi_199a$tdqbi1 - qbi_199a$cap2), (qbi_199a$tdqbi1 - qbi_199a$cap2)*((qbi_199a$ti - qbi_199a$exp_*qbi_199a$fs)/(50000*qbi_199a$fs))))
  }
  
  qbi_199a$tdqbi2 <- max(0, df$X2.8*0.2)  
  
  qbi_199a$cap3 <- max(qbi_199a$ceil1c, qbi_199a$ceil2c)
  
  if (qbi_199a$fs==0){
    qbi_199a$clb3 <- 0
  } else {
    qbi_199a$clb3 <- max(0, min((qbi_199a$tdqbi2 - qbi_199a$cap3), (qbi_199a$tdqbi2 - qbi_199a$cap3)*((qbi_199a$ti - qbi_199a$exp_*qbi_199a$fs)/(50000*qbi_199a$fs))))
  }
  
  
  qbi_199a$tdqbix <- max(0, (df$X2.10 - df$X2.12*(df$X2.10/(df$X2.10 + df$X2.11)))*0.2)
  qbi_199a$ceil1x <- df$X3.1e.1*0.5
  qbi_199a$ceil2x <- df$X3.1e.1*0.25 + df$X3.2e.1*0.025
  qbi_199a$capx <- max(qbi_199a$ceil1x, qbi_199a$ceil2x)
  qbi_199a$clbx <- max(0, min(qbi_199a$tdqbix - qbi_199a$capx, (qbi_199a$tdqbix - qbi_199a$capx)*((qbi_199a$ti - qbi_199a$exp_*qbi_199a$fs)/(50000*qbi_199a$fs))))
  
  
  qbi_199a$tdqbiy <- max(0, (df$X2.11 - df$X2.12*(df$X2.11/(df$X2.10 + df$X2.11)))*0.2)
  qbi_199a$ceil1y <- df$X3.1e.2*0.5
  qbi_199a$ceil2y <- df$X3.1e.2*0.25 + df$X3.2e.2*0.025
  qbi_199a$capy <- max(qbi_199a$ceil1y, qbi_199a$ceil2y)
  qbi_199a$clby <- max(0, min(qbi_199a$tdqbiy - qbi_199a$capy, (qbi_199a$tdqbiy - qbi_199a$capy)*((qbi_199a$ti - qbi_199a$exp_*qbi_199a$fs)/(50000*qbi_199a$fs))))
  
  
  if (df$X2.1_1=='Qualified payment from a specified cooperative' &
      df$X2.6=='Yes'){
    qbi_199a$dqbi <- max(0, qbi_199a$qbiqp - qbi_199a$clb - qbi_199a$rd)
  } else if (df$X2.1_1=='Qualified payment from a specified cooperative' &
             df$X2.6=='No'){
    qbi_199a$dqbi <- 0
  } else if (df$X2.1_1=='Income or loss from a qualified trade or business (QTB)' |
             df$X2.1_1=='Income or loss from multiple qualified trades or businesses (QTBs) eligible to be aggregated as a single trade or business'){
    qbi_199a$dqbi <- max(0, qbi_199a$tdqbi - qbi_199a$clb)
  } else if (df$X2.1_1=='Income or loss from a specified service trade or business (SSTB)'){
    qbi_199a$dqbi <- max(0, (qbi_199a$tdqbi - qbi_199a$clb)*qbi_199a$ap)
  } else if (df$X2.1_2=='The multiple trades or businesses involves a qualified trade or business (QTB) and qualified payment from a specified cooperative' &
             df$X2.6=='Yes'){
    qbi_199a$dqbi <- max(0, qbi_199a$qbiqp - qbi_199a$clb1 - qbi_199a$rd + qbi_199a$tdqbi1 - qbi_199a$clb2)
  } else if (df$X2.1_2=='The multiple trades or businesses involves a qualified trade or business (QTB) and qualified payment from a specified cooperative' &
             df$X2.6=='No'){
    qbi_199a$dqbi <- max(0, qbi_199a$tdqbi1 - qbi_199a$clb2)
  } else if (df$X2.1_2=='The multiple trades or business include a qualified trade or business (QTB) and a specified service trade or business (SSTB), the two of which can be separated from one another (i.e., by separate invoice etc.)'){
    qbi_199a$dqbi <- max(0, qbi_199a$tdqbi1 - qbi_199a$clb2 + (qbi_199a$tdqbi2 - qbi_199a$clb3)*qbi_199a$ap)
  } else if (df$X2.1_2=='The multiple trades or businesses involve qualified payment from a specified cooperative as well as income, gain, deduction and loss from a specified service trade or business operate directly by the specified cooperative' &
             df$X2.6=='Yes'){
    qbi_199a$dqbi <- max(0, qbi_199a$qbiqp - qbi_199a$clb1 - qbi_199a$rd + (qbi_199a$tdqbi2 - qbi_199a$clb3)*qbi_199a$ap)
  } else if (df$X2.1_2=='The multiple trades or businesses involve qualified payment from a specified cooperative as well as income, gain, deduction and loss from a specified service trade or business operate directly by the specified cooperative' &
             df$X2.6=='No'){
    qbi_199a$dqbi <- 0
  } else if (df$X2.1_2=='The multiple trades or businesses involve three qualified trades or businesses (QTBs), one of which is in loss in the tax year' &
             (df$X2.12 < (df$X2.11 + df$X2.12))){
    qbi_199a$dqbi <- max(0, qbi_199a$tdqbix - qbi_199a$clbx + qbi_199a$tdqbiy - qbi_199a$clby)
  } else if (df$X2.1_2=='The multiple trades or businesses involve three qualified trades or businesses (QTBs), one of which is in loss in the tax year' &
             (df$X2.12 >= (df$X2.11 + df$X2.12))){
    qbi_199a$dqbi <- 0            
  } else {
    qbi_199a$dqbi <- 0
  }
  
  qbi_199a$cqbi <- qbi_199a$qii*0.2 + qbi_199a$dqbi
  
  qbi_199a$qbid <- min(qbi_199a$lit, qbi_199a$cqbi)
  
  if (qbi_199a$exp_*qbi_199a$fs < qbi_199a$ti & qbi_199a$ti < (qbi_199a$exp_+50000)*qbi_199a$fs){
    qbi_199a$shortcut2 <- TRUE
  } else {
    qbi_199a$shortcut2 <- FALSE
  }
  
  
  if (df$X1.2=='A specified agricultural or horticultural cooperative (specified cooperative) reporting federal income tax on Form 1120-C' &
      qbi_199a$qbd > 0){
    qbi_199a$result <- 'Modified 199A deduction for the specified cooperative'
  } else if (df$X1.2=='A specified agricultural or horticultural cooperative (specified cooperative) reporting federal income tax on Form 1120-C' &
             qbi_199a$qbd == 0){
    qbi_199a$result <- 'No modified 199A deduction available for the specified cooperative'
  } else if ((df$X1.2=='An individual reporting federal income tax on Form 1040 or Form 1040NR' &
              qbi_199a$qbid == 0 &
              (qbi_199a$lit < qbi_199a$cqbi | (qbi_199a$lit == 0 & qbi_199a$cqbi == 0))) |
             (df$X1.2=='A trust or estate reporting federal income tax on Form 1041' &
              lit == 0 &
              (qbi_199a$lit < qbi_199a$cqbi | (qbi_199a$lit == 0 & qbi_199a$cqbi == 0)))){
    qbi_199a$result <- '199A deduction limited to zero'
  } else if ((df$X1.2=='An individual reporting federal income tax on Form 1040 or Form 1040NR' &
              qbi_199a$qbid == 0 &
              qbi_199a$cqbi < qbi_199a$lit) |
             (df$X1.2=='A trust or estate reporting federal income tax on Form 1041' &
              qbi_199a$qbid == 0 &
              qbi_199a$cqbi < qbi_199a$lit)){
    qbi_199a$result <- 'No 199A deduction available'
  } else if ((df$X1.2=='An individual reporting federal income tax on Form 1040 or Form 1040NR' &
              qbi_199a$lit < qbi_199a$cqbi &
              qbi_199a$qbid > 0) |
             (df$X1.2=='A trust or estate reporting federal income tax on Form 1041' &
              qbi_199a$lit < qbi_199a$cqbi &
              qbi_199a$qbid > 0)){
    qbi_199a$result <- 'The 199A deduction subject to limitation'
  } else if ((df$X1.2=='An individual reporting federal income tax on Form 1040 or Form 1040NR' &
              qbi_199a$shortcut1 == FALSE) |
             df$X1.2=='A trust or estate reporting federal income tax on Form 1041' &
             shortcut1 == FALSE){
    qbi_199a$result <- 'Full 199A deduction'
  } else if ((df$X1.2=='An individual reporting federal income tax on Form 1040 or Form 1040NR' &
              qbi_199a$lit  > qbi_199a$cqbi &
              qbi_199a$qbid > 0 & 
              qbi_199a$shortcut2==TRUE) |
             (df$X1.2=='A trust or estate reporting federal income tax on Form 1041' &
              qbi_199a$lit > qbi_199a$cqbi &
              qbi_199a$qbid > 0 &
              qbi_199a$shortcut2==TRUE)){
    qbi_199a$result <- 'The 199A deduction subject to phase-in'
  } else if ((df$X1.2=='An individual reporting federal income tax on Form 1040 or Form 1040NR' &
              qbi_199a$lit  > qbi_199a$cqbi &
              qbi_199a$qbid > 0 & 
              qbi_199a$shortcut1==TRUE &
              qbi_199a$shortcut2==FALSE) |
             (df$X1.2=='A trust or estate reporting federal income tax on Form 1041' &
              qbi_199a$lit > qbi_199a$cqbi &
              qbi_199a$qbid > 0 &
              qbi_199a$shortcut1==TRUE &
              qbi_199a$shortcut2==FALSE)){
    qbi_199a$result <- 'The 199A deduction subject to cap'
  } else {
    qbi_199a$result <- 'The 199A deduction cannot be determined'
  }
  
  qbi_199a$texp <- qbi_199a$exp_*qbi_199a$fs
  
  qbi_199a$tdqbix <- round(qbi_199a$tdqbix)
  qbi_199a$ceil1x <- round(qbi_199a$ceil1x)
  qbi_199a$ceil2x <- round(qbi_199a$ceil2x)
  qbi_199a$capx <- round(qbi_199a$capx)
  qbi_199a$clbx <- round(qbi_199a$clbx)
  qbi_199a$tdqbiy <- round(qbi_199a$tdqbiy)
  qbi_199a$ceil1y <- round(qbi_199a$ceil1y)
  qbi_199a$ceil2y <- round(qbi_199a$ceil2y)
  qbi_199a$capy <- round(qbi_199a$capy)
  qbi_199a$clby <- round(qbi_199a$clby)
  
  qbi_199a$dqbi <- round(qbi_199a$dqbi)
  
  return(list(
    prediction=qbi_199a$result,
    subResults=list(
      list(
        group='ap',
        prediction=qbi_199a$ap
      ),
      list(
        group='cap',
        prediction=qbi_199a$cap
      ),
      list(
        group='cap1',
        prediction=qbi_199a$cap1
      ),
      list(
        group='cap2',
        prediction=qbi_199a$cap2
      ),
      list(
        group='cap3',
        prediction=qbi_199a$cap3
      ),
      list(
        group='ceil1',
        prediction=qbi_199a$ceil1
      ),
      list(
        group='ceil1a',
        prediction=qbi_199a$ceil1a
      ),
      list(
        group='ceil1b',
        prediction=qbi_199a$ceil1b
      ),
      list(
        group='ceil1c',
        prediction=qbi_199a$ceil1c
      ),
      list(
        group='ceil2',
        prediction=qbi_199a$ceil2
      ),
      list(
        group='ceil2a',
        prediction=qbi_199a$ceil2a
      ),
      list(
        group='ceil2b',
        prediction=qbi_199a$ceil2b
      ),
      list(
        group='ceil2c',
        prediction=qbi_199a$ceil2c
      ),
      list(
        group='clb',
        prediction=qbi_199a$clb
      ),
      list(
        group='clb1',
        prediction=qbi_199a$clb1
      ),
      list(
        group='clb2',
        prediction=qbi_199a$clb2
      ),
      list(
        group='clb3',
        prediction=qbi_199a$clb3
      ),
      list(
        group='dpad',
        prediction=qbi_199a$dpad
      ),
      list(
        group='dqbi',
        prediction=qbi_199a$dqbi
      ),
      list(
        group='exp',
        prediction=qbi_199a$exp_
      ),
      list(
        group='fs',
        prediction=qbi_199a$fs
      ),
      list(
        group='lit',
        prediction=qbi_199a$lit
      ),
      list(
        group='qbd',
        prediction=qbi_199a$qbd
      ),
      list(
        group='qbid',
        prediction=qbi_199a$qbid
      ),
      list(
        group='qbiqp',
        prediction=qbi_199a$qbiqp
      ),
      list(
        group='qii',
        prediction=qbi_199a$qii
      ),
      list(
        group='rd',
        prediction=qbi_199a$rd
      ),
      list(
        group='tdqbi',
        prediction=qbi_199a$tdqbi
      ),
      list(
        group='tdqbi1',
        prediction=qbi_199a$tdqbi1
      ),
      list(
        group='tdqbi2',
        prediction=qbi_199a$tdqbi2
      ),
      list(
        group='texp',
        prediction=qbi_199a$texp
      ),
      list(
        group='tdqbix',
        prediction=qbi_199a$tdqbix
      ),
      list(
        group='ceil1x',
        prediction=qbi_199a$ceil1x
      ),
      list(
        group='ceil2x',
        prediction=qbi_199a$ceil2x
      ),
      list(
        group='capx',
        prediction=qbi_199a$capx
      ),
      list(
        group='clbx',
        prediction=qbi_199a$clbx
      ),
      list(
        group='tdqbiy',
        prediction=qbi_199a$tdqbiy
      ),
      list(
        group='ceil1y',
        prediction=qbi_199a$ceil1y
      ),
      list(
        group='ceil2y',
        prediction=qbi_199a$ceil2y
      ),
      list(
        group='capy',
        prediction=qbi_199a$capy
      ),
      list(
        group='clby',
        prediction=qbi_199a$clby
      )
    )
  ))

}

