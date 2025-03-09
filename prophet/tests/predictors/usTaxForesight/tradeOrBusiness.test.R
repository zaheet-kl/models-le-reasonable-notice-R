prophetSource('predictors', 'usTaxForesight', 'tradeOrBusiness.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X3.1.01'=factor('1'),
                 'X3.4'=factor('1'),
                 'X4.1.01'=factor('1'),
                 'X5.1'=factor('1'),
                 'X5.3'=factor('1'))

describe('trade_or_business_transform_data', {
  it('converts type of 3.1.01 to numeric', {
    expect_equal(is.numeric(trade_or_business_transform_data(df)$X3.1.01), TRUE)
  })
  
  it('converts type of 3.4 to numeric', {
    expect_equal(is.numeric(trade_or_business_transform_data(df)$X3.4), TRUE)
  })
  
  it('converts type of 4.1.01 to numeric', {
    expect_equal(is.numeric(trade_or_business_transform_data(df)$X4.1.01), TRUE)
  })
  
  it('converts type of 5.1 to numeric', {
    expect_equal(is.numeric(trade_or_business_transform_data(df)$X5.1), TRUE)
  })
  
  it('converts type of 5.3 to numeric', {
    expect_equal(is.numeric(trade_or_business_transform_data(df)$X5.3), TRUE)
  })
  
})
