prophetSource('predictors', 'usEmploymentForesight', 'leaveOfAbsence.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X1.2'=factor('1'),
                 'X3.1.2'=factor('8'),
                 'X3.2.1'=factor('16'),
                 'X3.3.2'=factor('4'),
                 'X3.6'=factor('5'))

# 20 day cutoff for employer's quick decision to termination
df$X3.6_cutoff <- as.numeric(as.factor(df$X3.6)) < 20
df$X3.6_cutoff[which(df$X3.6_cutoff)] <- 'true'
df$X3.6_cutoff[which(df$X3.6_cutoff != 'true')] <- 'false'

# total leave days asked for minus statutory requirements
df$Xtra <- as.numeric(as.character(df$X3.1.2)) + as.numeric(as.character(df$X3.2.1)) - as.numeric(as.character(df$X3.3.2))
df$Xtra <- log(df$Xtra)
df$Xtra[which(is.na(df$Xtra))] <- 1


describe('leave_of_absence_transform_data', {
  it('converts type of 1.2 to numeric', {
    expect_equal(is.numeric(leave_of_absence_transform_data(df)$X1.2), TRUE)
  })

  it('converts type of 3.1.2 to numeric', {
    expect_equal(is.numeric(leave_of_absence_transform_data(df)$X3.1.2), TRUE)
  })
  
  it('converts type of 3.2.1 to numeric', {
    expect_equal(is.numeric(leave_of_absence_transform_data(df)$X3.2.1), TRUE)
  })
  
  it('converts type of 3.3.2 to numeric', {
    expect_equal(is.numeric(leave_of_absence_transform_data(df)$X3.3.2), TRUE)
  })
  
  it('converts type of 3.6 to numeric', {
    expect_equal(is.numeric(leave_of_absence_transform_data(df)$X3.6), TRUE)
  })
  
  it('confirms logic for 3.6_cutoff', {
    expect_equal(leave_of_absence_transform_data(df)$X3.6_cutoff, df$X3.6_cutoff)
  })
  
  it('confirms value for Xtra', {
    expect_equal(leave_of_absence_transform_data(df)$Xtra, df$Xtra)
  })
  
})

df$X2.1.1.1 <- 'Mental condition'
df$X2.1_customChoiceValue = ''

describe('2.1.1.1 Mental Test', {
  
  it('confirms logic for mapping 2.1.1.1 mental', {
    expect_equal(leave_of_absence_transform_data(df)$X2.1_customChoiceValue, 'Mental')
  })
  
})

df$X2.1.1.1 <- 'Physical condition'
df$X2.1_customChoiceValue = ''

describe('2.1.1.1 Physical Test', {
  
  it('confirms logic for mapping 2.1.1.1 physical', {
    expect_equal(leave_of_absence_transform_data(df)$X2.1_customChoiceValue, 'Physical')
  })
  
})

df$X2.1.1.1 <- 'Both mental and physical conditions'
df$X2.1_customChoiceValue = ''

describe('2.1.1.1 Both Test', {
  
  it('confirms logic for mapping 2.1.1.1 both', {
    expect_equal(leave_of_absence_transform_data(df)$X2.1_customChoiceValue, 'Both')
  })
  
})

physical_df <- data.frame(X2.1.2 <- 'default')

describe('physical_category_factors_dala none', {
  
  it('checks physical_none logic', {
    expect_equal(physical_category_factors_dala(physical_df)$Xphysical_none, 'true')
  })
  
})

physical_df <- data.frame(X2.1.2 <- 'Musculoskeletal Injury')

describe('physical_category_factors_dala musculoskeletal', {
  
  it('checks musculoskeletal logic', {
    expect_equal(physical_category_factors_dala(physical_df)$Xmusculoskeletal, 'true')
  })
  
})

physical_df <- data.frame(X2.1.2 <- 'Neurological')

describe('physical_category_factors_dala neurological', {
  
  it('checks neurological logic', {
    expect_equal(physical_category_factors_dala(physical_df)$Xneurological, 'true')
  })
  
})
  
physical_df <- data.frame(X2.1.2 <- 'Autoimmune Disorder')

describe('physical_category_factors_dala autoimmune', {
  
  it('checks autoimmune logic', {
    expect_equal(physical_category_factors_dala(physical_df)$Xautoimmune, 'true')
  })
  
})
 
physical_df <- data.frame(X2.1.2 <- 'Other')

describe('physical_category_factors_dala other', {
  
  it('checks other logic', {
    expect_equal(physical_category_factors_dala(physical_df)$Xphysical_other, 'true')
  })
  
})


mental_df <- data.frame(X2.1.3 <- 'default')

describe('mental_category_factors_dala default', {
  
  it('checks mental default logic', {
    expect_equal(mental_category_factors_dala(mental_df)$Xmental_other, 'true')
  })
  
})


mental_df <- data.frame(X2.1.3 <- 'Alcoholism')

describe('mental_category_factors_dala alcoholism', {
  
  it('checks mental alcoholism logic', {
    expect_equal(mental_category_factors_dala(mental_df)$Xalcoholism, 'true')
  })
  
})

mental_df <- data.frame(X2.1.3 <- 'Anxiety')

describe('mental_category_factors_dala anxiety', {
  
  it('checks mental anxiety logic', {
    expect_equal(mental_category_factors_dala(mental_df)$Xanxiety, 'true')
  })
  
})

mental_df <- data.frame(X2.1.3 <- 'Other')

describe('mental_category_factors_dala other', {
  
  it('checks mental other logic', {
    expect_equal(mental_category_factors_dala(mental_df)$Xmental_other, 'true')
  })
  
})


describe('leave_of_absence_logic', {
  

  logic_df <- data.frame(X3.4.1 = NO)
    expect_equal(leave_of_absence_logic(logic_df)$probability, 1)
    expect_equal(leave_of_absence_logic(logic_df)$prediction, TRIABLE_ISSUE)
  
  logic_df <- data.frame(X3.3 = NO)
    expect_equal(leave_of_absence_logic(logic_df)$probability, 1)
    expect_equal(leave_of_absence_logic(logic_df)$prediction, TRIABLE_ISSUE)
  
  logic_df <- data.frame(X2.1_customChoiceValue = NEITHER,
                         X2.1.1 = NO)
    expect_equal(leave_of_absence_logic(logic_df)$probability, 1)
    expect_equal(leave_of_absence_logic(logic_df)$prediction, NOT_TRIABLE)
  
  logic_df <- data.frame(X3.3 = YES)
    expect_equal(leave_of_absence_logic(logic_df)$probability, NA)
    expect_equal(leave_of_absence_logic(logic_df)$prediction, '__noHardcode__')

})
