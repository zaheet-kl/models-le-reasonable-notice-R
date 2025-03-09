prophetSource('predictors', 'caTaxForesight', 'relatedPersonsAndDealingAtArmsLength.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings

df <- data.frame('X3.8'=factor('1'),
                 'X3.9'=factor('1'),
                 'X3.13'=factor('1'))

df <- data.frame('X3.8.1'=TRUE,
                 'X3.8.2'=TRUE,
                 'X3.8.3'=TRUE,
                 'X3.9.1'=TRUE,
                 'X3.9.2'=TRUE,
                 'X3.9.3'=TRUE,
                 'X3.9.4'=TRUE,
                 'X3.13.1'=TRUE,
                 'X3.13.2'=TRUE,
                 'X3.13.3'=TRUE,
                 'X3.13.4'=TRUE)


describe('related_persons_and_dealing_at_arms_length_transform_data', {
  it('converts type of 3.8 to numeric', {
    expect_equal(is.numeric(related_persons_and_dealing_at_arms_length_transform_data(df)$X3.8), TRUE)
  })
  
  it('converts type of 3.9 to numeric', {
    expect_equal(is.numeric(related_persons_and_dealing_at_arms_length_transform_data(df)$X3.9), TRUE)
  })
  
  it('converts type of 3.13 to numeric', {
    expect_equal(is.numeric(related_persons_and_dealing_at_arms_length_transform_data(df)$X3.13), TRUE)
  })

})

describe('related persons logic test', {
  
  logic_df <- data.frame('X2.1NA' = 'child',
                         'X2.1NA' = 'dependent',
                         'X2.1NA.1' = 'brother',
                         'X2.1NB' = 'married',
                         'X2.1NC' = 'partner',
                         'X2.1ND' = 'adopted',
                         'X2.1O1' = 'controls',
                         'X2.1O2' = 'controls',
                         'X2.1C' = 'controls',
                         'X2.2C' = 'controls',
                         'X2.3C.1' = 'shares',
                         '2.3C.2' = YES,
                         'X2.4C' = YES)


  
  logic_df <- related_persons_and_dealing_at_arms_length_transform_data(logic_df)

    
  expect_equal(RELATED, related_persons_and_dealing_at_arms_length_logic(logic_df)$prediction)
  expect_equal(1, related_persons_and_dealing_at_arms_length_logic(logic_df)$probability)
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df)$probabilityPrediction)
  
    
  logic_df <- data.frame('X1.0' = 'Yes, I only want to know if the persons are related',
                         'X1.2' = 'The other person is neither an individual natural person nor a corporation',
                         'X1.2.1' = 'Neither of them',
                         'X2.1N' = 'None of these categories describes the connection between the persons at all',
                         'X2.1NA.1' = 'None of the above descriptions apply',
                         'X2.1NB' = 'None of the above descriptions apply',
                         'X2.1NC' = 'None of the above descriptions apply',
                         'X2.1ND' = 'None of the above descriptions apply',
                         'X2.1O1' = 'None of the above descriptions apply',
                         'X2.1O2' = 'None of the above descriptions apply',
                         'X2.2C' = 'None of the above descriptions apply',
                         'X2.3C.1' = 'Neither of the above descriptions apply',
                         'X2.3C.2' = NOPE,
                         'X2.4C' = NOPE)


  logic_df <- related_persons_and_dealing_at_arms_length_transform_data(logic_df)

  
  expect_equal(UNRELATED, related_persons_and_dealing_at_arms_length_logic(logic_df)$prediction)
  expect_equal(1, related_persons_and_dealing_at_arms_length_logic(logic_df)$probability)
  expect_equal(ARM, related_persons_and_dealing_at_arms_length_logic(logic_df)$probabilityPrediction)
    
  logic_df <- data.frame('X3.0T.1.onehumanA' = YES,
                         'X3.0T.1.onehumanB' = 'None of the above are true',
                         'X3.0T.3.onehuman' = YES,
                         'X3.0T.4.onehuman' = YES,
                         'X3.0T.1.onecorpA' = 'None of the above are true',
                         'X3.0T.3.onecorp' = YES,
                         'X3.0T.4.onecorp' = YES,
                         'X3.0T.1.onetrustA' = 'None of the above are true',
                         'X3.0T.3.onetrust' = YES,
                         'X3.0T.4.onetrust' = YES,
                         'X3.0T.1.bothtrustA' = 'None of the above are true',
                         'X3.0T.2.bothtrustA' = YES,
                         'X3.0T.2.bothtrustB' = YES,
                         'X3.0T.3.bothtrustA' = YES,
                         'X3.0T.2.bothtrustB' = YES,
                         'X3.0P.1.onepartnership' = 'partnership',
                         'X3.0P.1.bothpartnership' = 'partnership',
                         'X3.0P.1.onehuman' = 'partnership',
                         'X3.0P.1.onecorp' = 'partnership')


  logic_df <- related_persons_and_dealing_at_arms_length_transform_data(logic_df)

    
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df)$prediction)
  expect_equal(1, related_persons_and_dealing_at_arms_length_logic(logic_df)$probability)
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df)$probabilityPrediction)
    
  logic_df <- data.frame('X1.0'=0)
  
  aic_sub = NOT_ACTING
  aic_prob = 1
  cm_sub = NOT_COM
  cm_prob = 0.5
  dfc_sub = NOT_DFC
  dfc_prob = 0
  
  expect_equal(ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$prediction)
  expect_equal(min(aic_prob, cm_prob, dfc_prob), related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probability)
  expect_equal(ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probabilityPrediction)
  
  logic_df <- data.frame('X1.0'=0)
  
  aic_sub = NOT_ACTING
  aic_prob = 0
  cm_sub = NOT_COM
  cm_prob = 1
  dfc_sub = NOT_DFC
  dfc_prob = 0.5
  
  expect_equal(ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$prediction)
  expect_equal(min(aic_prob, cm_prob, dfc_prob), related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probability)
  expect_equal(ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probabilityPrediction)
  
  logic_df <- data.frame('X1.0'=0)
  
  aic_sub = NOT_ACTING
  aic_prob = 0.5
  cm_sub = NOT_COM
  cm_prob = 0
  dfc_sub = NOT_DFC
  dfc_prob = 1
  
  expect_equal(ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$prediction)
  expect_equal(min(aic_prob, cm_prob, dfc_prob), related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probability)
  expect_equal(ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probabilityPrediction)
  
  
  logic_df <- data.frame('X1.0'=0)
  
  aic_sub = ACTING
  aic_prob = 1
  cm_sub = COM
  cm_prob = 0.5
  dfc_sub = DFC
  dfc_prob = 0
  
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$prediction)
  expect_equal(min(aic_prob, cm_prob, dfc_prob), related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probability)
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probabilityPrediction)
  
  logic_df <- data.frame('X1.0'=0)
  
  aic_sub = ACTING
  aic_prob = 0
  cm_sub = COM
  cm_prob = 1
  dfc_sub = DFC
  dfc_prob = 0.5
  
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$prediction)
  expect_equal(min(aic_prob, cm_prob, dfc_prob), related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probability)
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probabilityPrediction)
  
  logic_df <- data.frame('X1.0'=0)
  
  aic_sub = ACTING
  aic_prob = 0.5
  cm_sub = COM
  cm_prob = 0
  dfc_sub = DFC
  dfc_prob = 1
  
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$prediction)
  expect_equal(min(aic_prob, cm_prob, dfc_prob), related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probability)
  expect_equal(NOT_ARM, related_persons_and_dealing_at_arms_length_logic(logic_df, aic_sub, aic_prob, cm_sub, cm_prob, dfc_sub, dfc_prob)$probabilityPrediction)

})
