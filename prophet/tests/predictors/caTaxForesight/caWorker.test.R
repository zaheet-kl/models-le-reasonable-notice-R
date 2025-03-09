prophetSource('predictors', 'caTaxForesight', 'caWorker.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X1.4'=0,
                 'X1.3'=factor('No'),
                 'X2.1'=factor('No'),
                 'X2.2'=factor('No'),
                 'X2.3'=factor('No'),
                 'X2.4'=factor('No'),
                 'X2.5'=factor('No'),
                 'X2.6'=factor('No'),
                 'X2.7'=factor('No'),
                 'X2.8'=factor('No'),
                 'X3.2'=factor('No'),
                 'X4.1'=factor('No'),
                 'X4.3'=factor('No'),
                 'X5.1'=factor('No'),
                 'X5.2'=factor('No'))

describe('ca_worker_transform_data', {
  it('converts type of 1.4 to numeric', {
    expect_equal(is.numeric(ca_worker_transform_data(df)$X1.4), TRUE)
  })
  
  it('converts type of 1.3 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X1.3), TRUE)
  })
  
  it('converts type of 2.1 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.1), TRUE)
  })
  
  it('converts type of 2.2 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.2), TRUE)
  })
  
  it('converts type of 2.3 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.3), TRUE)
  })
  
  it('converts type of 2.4 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.4), TRUE)
  })
  
  it('converts type of 2.5 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.5), TRUE)
  })
  
  it('converts type of 2.6 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.6), TRUE)
  })
  
  it('converts type of 2.7 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.7), TRUE)
  })
  
  it('converts type of 2.8 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X2.8), TRUE)
  })
  
  it('converts type of 3.2 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X3.2), TRUE)
  })
  
  it('converts type of 4.1 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X4.1), TRUE)
  })
  
  it('converts type of 4.3 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X4.3), TRUE)
  })
  
  it('converts type of 5.1 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X5.1), TRUE)
  })
  
  it('converts type of 5.2 to factor', {
    expect_equal(is.factor(ca_worker_transform_data(df)$X5.2), TRUE)
  })
  
})