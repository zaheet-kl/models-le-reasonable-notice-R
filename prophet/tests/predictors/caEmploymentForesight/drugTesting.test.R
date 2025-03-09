prophetSource('predictors', 'caEmploymentForesight', 'drugTesting.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passed from blue-j-api to prophet as strings
df <- data.frame('X2.2'=factor("1"),
                 'X3.3'=factor("1"),
                 'X4.1'=factor("1"))

describe('drug_testing_transform_data', {
  
  it('converts type of 2.2 to numeric', {
    expect_equal(is.numeric(drug_testing_transform_data(df)$X2.2), TRUE)
  })
  
  it('converts type of 3.3 to numeric', {
    expect_equal(is.numeric(drug_testing_transform_data(df)$X3.3), TRUE)
  })
  
  it('converts type of 4.1 to numeric', {
    expect_equal(is.numeric(drug_testing_transform_data(df)$X4.1), TRUE)
  })
  
})
