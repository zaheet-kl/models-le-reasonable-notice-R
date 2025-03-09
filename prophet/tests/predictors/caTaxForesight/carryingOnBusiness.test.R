prophetSource('predictors', 'caTaxForesight', 'carryingOnBusiness.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X1.1_customChoiceValue'=factor('Corporation'),
                 'X4.1'=factor('Recurring or continuous activity in Canada'),
                 'X4.2'=factor('9'),
                 "1.3"=factor(NO),
                 "1.4"=factor(NO),
                 "4.3.1"=factor(NO),
                 "4.3.2"=factor(NO),
                 "4.3.3"=factor(NO),
                 "4.3.4"=factor(NO),
                 "4.3.5"=factor(NO),
                 "4.3.6"=factor(NO),
                 "4.3.7"=factor(NO),
                 "4.3.8"=factor(NO),
                 "4.3.9"=factor(NO),
                 "4.3.10"=factor(NO),
                 "4.3.11"=factor(NO),
                 "4.3.12"=factor(NO),
                 "4.4"=factor(NO),
                 "4.5"=factor(NO),
                 "4.6"=factor(NO),
                 "5.1"=factor(NO),
                 "5.2"=factor(NO),
                 "5.3"=factor(NO),
                 "5.4"=factor(NO),
                 "0.1"=factor(NO),
                 "1.3"=factor(NO),
                 "1.4"=factor(NO),
                 "2.1"=factor(NO),
                 "2.2"=factor(NO),
                 "2.3"=factor(NO),
                 "2.4"=factor(NO),
                 "3.2"=factor(NO),
                 "3.3"=factor(NO),
                 "3.4"=factor(NO),
                 "3.5"=factor(NO),
                 "3.6"=factor(NO),
                 "3.7"=factor(NO),
                 "3.8"=factor(NO),
                 "3.1.1"=factor(NO),
                 "3.1.2"=factor(NO),
                 "3.1.3"=factor(NO),
                 "3.1.4"=factor(NO),
                 "3.1.5"=factor(NO),
                 "3.1.6"=factor(NO),
                 "3.1.7"=factor(NO),
                 "3.1.8"=factor(NO),
                 "3.1.9"=factor(NO),
                 "3.1.10"=factor(NO))

describe('carrying_on_business_transform_data', {
  it('converts type of 1.1_ccv to factor', {
    expect_equal(is.factor(carrying_on_business_transform_data(df)$X1.1_customChoiceValue), TRUE)
  })

  it('converts type of 4.1 to factor', {
    expect_equal(is.factor(carrying_on_business_transform_data(df)$X4.1), TRUE)
  })
  
  it('converts type of 4.2 to numeric', {
    expect_equal(is.numeric(carrying_on_business_transform_data(df)$X4.2), TRUE)
  })
  
})


control <- 0

logic_df <- data.frame('X1.3' = NO,
                       'X1.4' = NO,
                       'X2.1' = NO,
                       'X2.2' = NO,
                       'X2.3' = NO,
                       'X2.4' = NO)

describe('carrying_on_business_logic test 1', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_CARRYING_ON_BUSINESS, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$prediction)
    expect_equal(1, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$probability)
    
  })
  
})
  
logic_df <- data.frame('X0.1' = ITA_COB,
                       'X1.3' = YES,
                       'X1.4' = YES,
                       'X3.2' = YES,
                       'X3.3' = YES,
                       'X3.4' = YES,
                       'X3.5' = YES,
                       'X3.6' = NO)

describe('carrying_on_business_logic  test 2', {
  
  it('tests hardcode logic', {
    
    expect_equal(CARRYING_ON_BUSINESS, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$prediction)
    expect_equal(1, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$probability)
    
  })
  
})
  
logic_df <- data.frame('X0.1' = ETA_COB,
           'X1.3' = YES,
           'X1.4' = YES,
           'X3.7' = YES,
           'X3.8' = NO)

describe('carrying_on_business_logic test 3', {
  
  it('tests hardcode logic', {
    
    expect_equal(CARRYING_ON_BUSINESS, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$prediction)
    expect_equal(1, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$probability)
    
  })
  
})
  
logic_df <- data.frame('X1.3' = YES,
                       'X1.4' = YES)

describe('carrying_on_business_logic test 4', {
  
  it('tests hardcode logic', {
    
    expect_equal(NO_HARDCODE, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$prediction)
    expect_equal(1, carrying_on_business_logic(logic_df, carrying_on_business_control(logic_df))$probability)
    
  })
  
})