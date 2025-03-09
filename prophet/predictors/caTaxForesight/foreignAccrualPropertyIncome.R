foreign_accrual_property_income_factors <- function(){
  return(c(
    '1.1_1',
    '2.1D.3',
    '2.1IN.1',
    '2.1CD.2',
    '2.1PD.1',
    '2.1SV.1',
    '3.1A',
    '3.1D',
    '3.1F',
    '3.1IN',
    '3.1P',
    '3.1TD',
    '3.2.01',
    '3.2_1',
    '4.1.3',
    '4.2.2',
    '4.3P.a',
    '4.3P.b',
    '4.3P.b.1',
    '4.3P.c',
    '4.3P.d',
    '4.3D.1',
    '4.3F.1',
    '4.3H.1',
    '4.3L.1',
    '5.2',
    '5.2.1a',
    '5.2.1b',
    '5.2.1c',
    '5.2.1d',
    '5.2.1e',
    '5.2.1f',
    '5.2.01',
    '5.3.01',
    '5.3_1',
    '6.1.1',
    '6.1.2',
    '6.2.1',
    '6.2.2',
    '7.2',
    '7.2.02',
    '7.2.03',
    '7.2.1',
    '7.2.2',
    '7.3.1a',
    '7.3.1b',
    '7.3.1c',
    '7.3.1d',
    '7.4'
  ))
}



foreign_accrual_property_income_predict <- function(df){
  
  df <- format_dataframe_numerics(df, c(
    '1.1_1',
    '2.1D.3',
    '2.1IN.1',
    '2.1CD.2',
    '2.1PD.1',
    '2.1SV.1',
    '3.1A',
    '3.1D',
    '3.1F',
    '3.1IN',
    '3.1P',
    '3.1TD',
    '3.2.01',
    '3.2_1',
    '4.1.3',
    '4.2.2',
    '4.3P.a',
    '4.3P.b',
    '4.3P.b.1',
    '4.3P.c',
    '4.3P.d',
    '4.3D.1',
    '4.3F.1',
    '4.3H.1',
    '4.3L.1',
    '5.2',
    '5.2.1a',
    '5.2.1b',
    '5.2.1c',
    '5.2.1d',
    '5.2.1e',
    '5.2.1f',
    '5.2.01',
    '5.3.01',
    '5.3_1',
    '6.1.1',
    '6.1.2',
    '6.2.1',
    '6.2.2',
    '7.2',
    '7.2.02',
    '7.2.03',
    '7.2.1',
    '7.2.2',
    '7.3.1a',
    '7.3.1b',
    '7.3.1c',
    '7.3.1d',
    '7.4'
  ))
  
  df[which(is.na(df)==T)] <- 'default'
  
  fapi <- data.frame(inq = '',
                     lnq = '',
                     idotab = '',
                     ldotab = '',
                     ip = '',
                     lp = '',
                     fapl_carryover = '',
                     idab = '',
                     ldab = '',
                     cg = '',
                     cl = '',
                     facl_carryover = '',
                     oif = '',
                     d_f = '',
                     pp = '',
                     fat = '',
                     a = '',
                     a.1 = '',
                     a.2 = '',
                     b = '',
                     c = '',
                     d = '',
                     e = '',
                     f = '',
                     f.1 = '',
                     g = '',
                     h = '',
                     total_fapi = '',
                     included_income = '',
                     fat_deduction = '',
                     net_included_income = '',
                     t1134i = '',
                     t1134ii = '',
                     t1134iii = '',
                     t1134iv = '',
                     t1134v = '',
                     t1134vi = '',
                     t1134vii = '',
                     t1134viii = ''
                     
                     )
  
  
  if (df$'1.1_1' > 0){
    fapi$inq <- fapi$inq + df$'1.1_1'
  } else {
    fapi$inq <- 0
  }
  
  
  if (df$'1.1_1' < 0){
    fapi$lnq <- fapi$lnq + df$'1.1_1'
  } else {
    fapi$lnq <- 0
  }
  
  
  if (df$'2.1D.3' > 0){
    fapi$idotab <- fapi$idotab + df$'2.1D.3'
  }
  if (df$'2.1IN.1' > 0){
    fapi$idotab <- fapi$idotab + df$'2.1IN.1'
  }
  if (df$'2.1CD.2' > 0){
    fapi$idotab <- fapi$idotab + df$'2.1CD.2'
  }
  if (df$'2.1PD.1' > 0){
    fapi$idotab <- fapi$idotab + df$'2.1PD.1'
  }
  if (df$'2.1SV.1' > 0){
    fapi$idotab <- fapi$idotab + df$'2.1SV.1'
  }
  
  
  if (df$'2.1D.3' < 0){
    fapi$ldotab <- fapi$ldotab + df$'2.1D.3'
  }
  if (df$'2.1IN.1' < 0){
    fapi$ldotab <- fapi$ldotab + df$'2.1IN.1'
  }
  if (df$'2.1CD.2' < 0){
    fapi$ldotab <- fapi$ldotab + df$'2.1CD.2'
  }
  if (df$'2.1PD.1' < 0){
    fapi$ldotab <- fapi$ldotab + df$'2.1PD.1'
  }
  if (df$'2.1SV.1' < 0){
    fapi$ldotab <- fapi$ldotab + df$'2.1SV.1'
  }
  
  
  if (df$'3.1A' > 0){
    fapi$ip <- fapi$ip + df$'3.1A'
  }
  if (df$'3.1D' > 0){
    fapi$ip <- fapi$ip + df$'3.1D'
  }
  if (df$'3.1F' > 0){
    fapi$ip <- fapi$ip + df$'3.1F'
  }
  if (df$'3.1IN' > 0){
    fapi$ip <- fapi$ip + df$'3.1IN'
  }
  if (df$'3.1P' > 0){
    fapi$ip <- fapi$ip + df$'3.1P'
  }
  if (df$'3.1TD' > 0){
    fapi$ip <- fapi$ip + df$'3.1TD'
  }
  
  
  if (df$'3.1A' < 0){
    fapi$lp <- fapi$lp + df$'3.1A'
  }
  if (df$'3.1D' < 0){
    fapi$lp <- fapi$lp + df$'3.1D'
  }
  if (df$'3.1F' < 0){
    fapi$lp <- fapi$lp + df$'3.1F'
  }
  if (df$'3.1IN' < 0){
    fapi$lp <- fapi$lp + df$'3.1IN'
  }
  if (df$'3.1P' < 0){
    fapi$lp <- fapi$lp + df$'3.1P'
  }
  if (df$'3.1TD' < 0){
    fapi$lp <- fapi$lp + df$'3.1TD'
  }
  
  
  fapi$fapl_carryover <- abs(df$'3.2.01' + df$'3.2_1')
  
  
  if (df$'4.1.3' > 0){
    fapi$idab <- fapi$idab + df$'4.1.3'
  }
  if (df$'4.2.2' > 0){
    fapi$idab <- fapi$idab + df$'4.2.2'
  }
  if (df$'4.3P.a' > 0){
    fapi$idab <- fapi$idab + df$'4.3P.a'
  }
  if (df$'4.3P.b' > 0){
    fapi$idab <- fapi$idab + df$'4.3P.b'
  }
  if (df$'4.3P.b.1' > 0){
    fapi$idab <- fapi$idab + df$'4.3P.b.1'
  }
  if (df$'4.3P.c' > 0){
    fapi$idab <- fapi$idab + df$'4.3P.c'
  }
  if (df$'4.3P.d' > 0){
    fapi$idab <- fapi$idab + df$'4.3P.d'
  }
  if (df$'4.3D' > 0){
    fapi$idab <- fapi$idab + df$'4.3D'
  }
  if (df$'4.3F' > 0){
    fapi$idab <- fapi$idab + df$'4.3F'
  }
  if (df$'4.3H' > 0){
    fapi$idab <- fapi$idab + df$'4.3H'
  }
  if (df$'4.3L' > 0){
    fapi$idab <- fapi$idab + df$'4.3L'
  }
  
  
  if (df$'4.1.3' < 0){
    fapi$ldab <- fapi$ldab + df$'4.1.3'
  }
  if (df$'4.2.2' < 0){
    fapi$ldab <- fapi$ldab + df$'4.2.2'
  }
  if (df$'4.3P.a' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3P.a'
  }
  if (df$'4.3P.b' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3P.b'
  }
  if (df$'4.3P.b.1' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3P.b.1'
  }
  if (df$'4.3P.c' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3P.c'
  }
  if (df$'4.3P.d' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3P.d'
  }
  if (df$'4.3D' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3D'
  }
  if (df$'4.3F' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3F'
  }
  if (df$'4.3H' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3H'
  }
  if (df$'4.3L' < 0){
    fapi$ldab <- fapi$ldab + df$'4.3L'
  }
  
  
  fapi$cg <- df$'5.2'
  if (df$'5.2.1a' > 0){
    fapi$cg <- fapi$cg - df$'5.2.1a'
  }
  if (df$'5.2.1b' > 0){
    fapi$cg <- fapi$cg - df$'5.2.1b'
  }
  if (df$'5.2.1c' > 0){
    fapi$cg <- fapi$cg - df$'5.2.1c'
  }
  if (df$'5.2.1d' > 0){
    fapi$cg <- fapi$cg - df$'5.2.1d'
  }
  if (df$'5.2.1e' > 0){
    fapi$cg <- fapi$cg - df$'5.2.1e'
  }
  if (df$'5.2.1f' > 0){
    fapi$cg <- fapi$cg - df$'5.2.1f'
  }
  fapi$cg <- max(0, fapi$cg)
  
  
  fapi$cl <- df$'5.2.01'
  if (df$'5.2.1a' < 0){
    fapi$cl <- fapi$cl - df$'5.2.1a'
  }
  if (df$'5.2.1b' < 0){
    fapi$cl <- fapi$cl - df$'5.2.1b'
  }
  if (df$'5.2.1c' < 0){
    fapi$cl <- fapi$cl - df$'5.2.1c'
  }
  if (df$'5.2.1d' < 0){
    fapi$cl <- fapi$cl - df$'5.2.1d'
  }
  if (df$'5.2.1e' < 0){
    fapi$cl <- fapi$cl - df$'5.2.1e'
  }
  if (df$'5.2.1f' < 0){
    fapi$cl <- fapi$cl - df$'5.2.1f'
  }
  fapi$cl <- min(0, fapi$cl)
  
  
  fapi$facl_carryover <- abs(df$'5.3.01' + df$'5.3_1')
  
  
  fapi$oif <- max(0, df$'6.1.1' - df$'6.1.2')
  
  fapi$d_f <- 0
  
  fapi$pp <- max(df$'7.2', df$'7.2.02' + df$'7.2.03', df$'7.2.1' + df$'7.2.2')/100
  
  fapi$fat <- df$'7.3.1a' + df$'7.3.1b' + df$'7.3.1c' + df$'7.3.1d'
  
  fapi$a <- max(0, fapi$inq + fapi$ip - fapi$idab + fapi$idotab)
  
  fapi$a.1 <- 2*df$'6.2.1'
  
  fapi$a.2 <- df$'6.2.2'
  
  fapi$b <- max(0, 0.5*fapi$cg)
  
  fapi$c <- max(0, fapi$oif)
  
  fapi$d <- abs(min(0, fapi$lnq + fapi$lp - fapi$ldab + fapi$ldotab))
  
  fapi$e <- min(0.5*fapi$cg, abs(0.5*fapi$cl))
  
  fapi$f <- fapi$fapl_carryover
  
  fapi$f.1 <- min(max(0, 0.5*fapi$cg + 0.5*fapi$cl), fapi$facl_carryover)
  
  fapi$g <- max(0, fapi$a.1 + fapi$a.2 - fapi$d - fapi$e - fapi$f - fapi$f.1)
  
  fapi$h <- min(0, df$'4.3P.b.1')
  
  fapi$total_fapi <- max(0, fapi$a + fapi$a.1 + fapi$a.2 + fapi$b + fapi$c - fapi$d - fapi$e - fapi$f - fapi$f.1 - fapi$g - fapi$h)
  
  fapi$included_income <- max(0, fapi$total_fapi*pp)
  
  fapi$fat_deduction <- min(fapi$fat*df$'7.4', max(0, fapi$included_income - fapi$fat))
  
  fapi$net_included_income <- max(0, fapi$included_income - fapi$fat_deduction)
  
  fapi$t1134i <- max(0, fapi$ip - abs(fapi$lp) - fapi$idab + fapi$abs(fapi$ldab) - fapi$fapl_carryover)
  
  fapi$t1134ii <- max(0, df$'2.1D.3')
  
  fapi$t1134iii <- max(0, df$'2.1IN.1')
  
  fapi$t1134iv <- max(0, df$'2.1CD.2')
  
  fapi$t1134v <- max(0, df$'2.1PD.1')
  
  fapi$t1134vi <- max(0, df$'2.1SV.1')
  
  fapi$t1134vii <- max(0, fapi$b - fapi$e - fapi$f.1)
  
  fapi$t1134viii <- max(0, fapi$oif)
  
  if (fapi$total_fapi == 0){
    fapi$result <- 'No FAPI'
  } else if (fapi$total_fapi > 0 & fapi$total_fapi <=5000){
    fapi$result <- 'De Minimis FAPI'
  } else if (fapi$total_fapi > 5000){
    fapi$result <- 'FAPI'
  } else {
    fapi$result <- 'Insufficient Information'
  }
  
  return(list(
    prediction=fapi$result,
    subResults=list(
      list(
        group='inq',
        prediction=fapi$inq
      ),
      list(
        group='lnq',
        prediction=fapi$lnq
      ),
      list(
        group='idotab',
        prediction=fapi$idotab
      ),
      list(
        group='ldotab',
        prediction=fapi$ldotab
      ),
      list(
        group='ip',
        prediction=fapi$ip
      ),
      list(
        group='lp',
        prediction=fapi$lp
      ),
      list(
        group='fapl_carryover',
        prediction=fapi$fapl_carryover
      ),
      list(
        group='idab',
        prediction=fapi$idab
      ),
      list(
        group='ldab',
        prediction=fapi$ldab
      ),
      list(
        group='cg',
        prediction=fapi$cg
      ),
      list(
        group='cl',
        prediction=fapi$cl
      ),
      list(
        group='facl_carryover',
        prediction=fapi$facl_carryover
      ),
      list(
        group='oif',
        prediction=fapi$oif
      ),
      list(
        group='df',
        prediction=fapi$d_f
      ),
      list(
        group='pp',
        prediction=fapi$pp
      ),
      list(
        group='fat',
        prediction=fapi$fat
      ),
      list(
        group='fat_deduction',
        prediction=fapi$fat_deduction
      ),
      list(
        group='a',
        prediction=fapi$a
      ),
      list(
        group='a.1',
        prediction=fapi$a.1
      ),
      list(
        group='a.2',
        prediction=fapi$a.2
      ),
      list(
        group='b',
        prediction=fapi$b
      ),
      list(
        group='c',
        prediction=fapi$c
      ),
      list(
        group='d',
        prediction=fapi$d
      ),
      list(
        group='e',
        prediction=fapi$e
      ),
      list(
        group='f',
        prediction=fapi$f
      ),
      list(
        group='f.1',
        prediction=fapi$f.1
      ),
      list(
        group='g',
        prediction=fapi$g
      ),
      list(
        group='h',
        prediction=fapi$h
      ),
      list(
        group='total_fapi',
        prediction=fapi$total_fapi
      ),
      list(
        group='included_income',
        prediction=fapi$included_income
      ),
      list(
        group='net_included_income',
        prediction=fapi$net_included_income
      ),
      list(
        group='t1134i',
        prediction=fapi$t1134i
      ),
      list(
        group='t1134ii',
        prediction=fapi$t1134ii
      ),
      list(
        group='t1134iii',
        prediction=fapi$t1134iii
      ),
      list(
        group='t1134iv',
        prediction=fapi$t1134iv
      ),
      list(
        group='t1134v',
        prediction=fapi$t1134v
      ),
      list(
        group='t1134vi',
        prediction=fapi$t1134vi
      ),
      list(
        group='t1134vii',
        prediction=fapi$t1134vii
      ),
      list(
        group='t1134viii',
        prediction=fapi$t1134viii
      )
    )
  ))
}
