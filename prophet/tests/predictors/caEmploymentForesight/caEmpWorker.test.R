prophetSource('predictors', 'caEmploymentForesight', 'caEmpWorker.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passed from blue-j-api to prophet as strings
df <- data.frame('X1.5'=factor("1"),
                 'X4.1'=factor("1"),
                 'X4.2'=factor("1"))

describe('ca_emp_worker_transform_data', {
  
  it('converts type of 1.5 to numeric', {
    expect_equal(is.numeric(ca_emp_worker_transform_data(df)$X1.5), TRUE)
  })
  
  it('converts type of 4.1 to numeric', {
    expect_equal(is.numeric(ca_emp_worker_transform_data(df)$X4.1), TRUE)
  })
  
  it('converts type of 4.2 to numeric', {
    expect_equal(is.numeric(ca_emp_worker_transform_data(df)$X4.2), TRUE)
  })
  
})
