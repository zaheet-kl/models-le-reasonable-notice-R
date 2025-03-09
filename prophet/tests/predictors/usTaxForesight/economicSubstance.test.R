prophetSource('predictors', 'usTaxForesight', 'economicSubstance.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X5.1'= factor(1),
                 'X1.7' = factor(1),
                 'X3.1' = factor(1),
                 'X3.3' = factor(1),
                 'X3.4' = factor(1),
                 'X3.7.1' = factor(1),
                 'X5.2.1' = factor(1),
                 'X5.2.2' = factor(1))
                 
describe('economic_substance_transform_data', {
  it('converts type of 1.7 to numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$X1.7), TRUE)
  })
  
  it('converts type of 3.1 to numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$X3.1), TRUE)
  })
  
  it('converts type of 3.3 to numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$X3.3), TRUE)
  })
  
  it('converts type of 3.4 to numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$X3.4), TRUE)
  })
  
  it('converts type of 3.7.1 to numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$X3.7.1), TRUE)
  })
  
  it('converts type of 5.2.1 to numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$X5.2.1), TRUE)
  })
  
  it('converts type of 5.2.2 to numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$X5.2.2), TRUE)
  })
  
  it('confirms ROI is numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$ROI), TRUE)
  })
  
  it('confirms LTP is numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$LossToProfit), TRUE)
  })
  
  it('confirms STP is numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$SavingsToProfit), TRUE)
  })
  
  it('confirms STR is numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$SavingsToRisk), TRUE)
  })
  
  it('confirms CTP is numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$CreditsToProfit), TRUE)
  })
  
  it('confirms CTR is numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$CreditsToRisk), TRUE)
  })
  
  it('confirms NRTR is numeric', {
    expect_equal(is.numeric(economic_substance_transform_data(df)$NoRiskToRisk), TRUE)
  })

})
