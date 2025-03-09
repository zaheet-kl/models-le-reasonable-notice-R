prophetSource('predictors', 'caTaxForesight', 'grossNegligence.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passed from blue-j-api to prophet as strings
df <- data.frame('X1.3'=1,
                 'X2.4'=1,
                 'X3.2'=1,
                 'X3.3'=1)

describe('gross_negligence_transform_data', {
  it('converts type of 1.3 to numeric', {
    expect_equal(is.numeric(gross_negligence_transform_data(df)$X1.3), TRUE)
  })
  
  it('converts type of 2.4 to numeric', {
    expect_equal(is.numeric(gross_negligence_transform_data(df)$X2.4), TRUE)
  })
  
  it('converts type of 3.2 to numeric', {
    expect_equal(is.numeric(gross_negligence_transform_data(df)$X3.2), TRUE)
  })
  
  it('converts type of 3.3 to numeric', {
    expect_equal(is.numeric(gross_negligence_transform_data(df)$X3.3), TRUE)
  })
  
  it('confirms value of 3.2log', {
    expect_equal(gross_negligence_transform_data(df)$X3.2log, log(df$X3.2 + 1))
  })
  
  it('confirms value of 3.3log', {
    expect_equal(gross_negligence_transform_data(df)$X3.3log, log(df$X3.3 + 1))
  })
  
})