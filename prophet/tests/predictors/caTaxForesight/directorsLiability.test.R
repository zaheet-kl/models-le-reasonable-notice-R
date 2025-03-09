prophetSource('predictors', 'caTaxForesight', 'directorsLiability.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passed from blue-j-api to prophet as strings
df <- data.frame('X1.3'=1,
                 'X4.2'=1)

describe('directors_liability_transform_data', {
  it('converts type of 1.3 to numeric', {
    expect_equal(is.numeric(directors_liability_transform_data(df)$X1.3), TRUE)
  })
  
  it('converts type of 4.2 to numeric', {
    expect_equal(is.numeric(directors_liability_transform_data(df)$X4.2), TRUE)
  })
  
  it('confirms value of 4.2log', {
    expect_equal(directors_liability_transform_data(df)$X4.2log, log(df$X4.2))
  })
  
})