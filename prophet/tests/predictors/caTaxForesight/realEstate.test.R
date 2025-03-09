prophetSource('predictors', 'caTaxForesight', 'realEstate.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passed from blue-j-api to prophet as strings
df <- data.frame('X2.6'=1,
                 'X3.4'=1,
                 'X4.2'=1,
                 'X5.2'=1)

describe('real_estate_ca_transform_data', {
  it('converts type of 2.6 to numeric', {
    expect_equal(is.numeric(real_estate_ca_transform_data(df)$X2.6), TRUE)
  })
  
  it('converts type of 3.4 to numeric', {
    expect_equal(is.numeric(real_estate_ca_transform_data(df)$X3.4), TRUE)
  })
  
  it('converts type of 4.2 to numeric', {
    expect_equal(is.numeric(real_estate_ca_transform_data(df)$X4.2), TRUE)
  })
  
  it('confirms value of 5.2plusonelog', {
    expect_equal(real_estate_ca_transform_data(df)$X5.2log, log(df$X5.2 + 1))
  })
  
})
