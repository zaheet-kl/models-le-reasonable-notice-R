prophetSource('predictors', 'caTaxForesight', 'windfall.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X1.3'=factor('1'),
                 'X2.2'=factor('1'),
                 'X2.3'=factor('1'),
                 'X3.1'=factor('1'))

describe('windfall_transform_data', {
  it('converts type of 1.3 to numeric', {
    expect_equal(is.numeric(windfall_transform_data(df)$X1.3), TRUE)
  })
  
  it('converts type of 2.2 to numeric', {
    expect_equal(is.numeric(windfall_transform_data(df)$X2.2), TRUE)
  })
  
  it('converts type of 2.3 to numeric', {
    expect_equal(is.numeric(windfall_transform_data(df)$X2.3), TRUE)
  })
  
  it('converts type of 3.1 to numeric', {
    expect_equal(is.numeric(windfall_transform_data(df)$X3.1), TRUE)
  })
  
})