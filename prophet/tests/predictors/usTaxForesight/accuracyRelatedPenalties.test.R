prophetSource('predictors', 'usTaxForesight', 'accuracyRelatedPenalties.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X2.1.01'=factor('1'),
                 'X2.5'=factor('1'),
                 'X4.4'=factor('1'))

describe('accuracy_related_penalties_transform_data', {
  it('converts type of 2.1.01 to numeric', {
    expect_equal(is.numeric(accuracy_related_penalties_transform_data(df)$X2.1.01), TRUE)
  })
  
  it('converts type of 2.5 to numeric', {
    expect_equal(is.numeric(accuracy_related_penalties_transform_data(df)$X2.5), TRUE)
  })
  
  it('converts type of 4.4 to numeric', {
    expect_equal(is.numeric(accuracy_related_penalties_transform_data(df)$X4.4), TRUE)
  })
  
})

describe('accuracy related penalties logic test', {
  
  logic_df <- data.frame(X1.1 = LACKING_ECONOMIC_SUBSTANCE)
    expect_equal(NO_RCGF, accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(0, accuracy_related_penalties_logic(logic_df)$probability)
    
  logic_df <- data.frame('X1.3' = 'No')
    expect_equal(NO_RCGF, accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(0, accuracy_related_penalties_logic(logic_df)$probability)
    
  logic_df <- data.frame('X1.4' = 'Yes')
    expect_equal(RCGF, accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(1, accuracy_related_penalties_logic(logic_df)$probability)
    
  logic_df <- data.frame('X1.5' = 'No')
    expect_equal(NO_RCGF, accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(0, accuracy_related_penalties_logic(logic_df)$probability)
    
  logic_df <- data.frame('X1.6' = CHARITABLE_DEDUCTION_PROPERTY)
    expect_equal(NO_RCGF, accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(0, accuracy_related_penalties_logic(logic_df)$probability)
    
  logic_df <- data.frame('X1.7' = 'Yes')
    expect_equal(RCGF, accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(1, accuracy_related_penalties_logic(logic_df)$probability)
    
  logic_df <- data.frame('X1.8' = 'No')
    expect_equal(NO_RCGF, accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(0, accuracy_related_penalties_logic(logic_df)$probability)
  
  logic_df <- data.frame('X1.8' = 'Yes')
    expect_equal('__noHardcode__', accuracy_related_penalties_logic(logic_df)$prediction)
    expect_equal(NA, accuracy_related_penalties_logic(logic_df)$probability)
    
})