prophetSource('predictors', 'usTaxForesight', 'stepTransactionDoctrine.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X1.4.1'=factor('true'),
                 'X1.4.2'=factor('true'),
                 'X1.4.3'=factor('true'))

describe('accuracy_related_penalties_transform_data', {
  it('converts type of 1.4.1 to factor', {
    expect_equal(is.factor(step_transaction_doctrine_transform_data(df)$X1.4.1), TRUE)
  })
  
  it('converts type of 1.4.2 to factor', {
    expect_equal(is.factor(step_transaction_doctrine_transform_data(df)$X1.4.2), TRUE)
  })
  
  it('converts type of 1.4.3 to factor', {
    expect_equal(is.factor(step_transaction_doctrine_transform_data(df)$X1.4.3), TRUE)
  })
  
})
