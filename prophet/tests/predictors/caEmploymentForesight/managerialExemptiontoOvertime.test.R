prophetSource('predictors', 'caEmploymentForesight', 'managerialExemptiontoOvertime.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passed from blue-j-api to prophet as strings
df <- data.frame('X3.3'=factor("1"),
                 'X3.4'=factor("1"))

describe('managerial_overtime_transform_data', {
  
  it('converts type of 3.3 to numeric', {
    expect_equal(is.numeric(managerial_overtime_transform_data(df)$X3.3), TRUE)
  })
  
  it('converts type of 3.4 to numeric', {
    expect_equal(is.numeric(managerial_overtime_transform_data(df)$X3.4), TRUE)
  })
  
})


df <- data.frame('X1.1'='Quebec')

describe('managerial_overtime_transform_data Quebec true test', {
  
  it('converts Quebec to true', {
    expect_equal(managerial_overtime_transform_data(df)$X1.1Quebec, TRUE)
  })
  
})


df <- data.frame('X1.1'='Ontario')

describe('managerial_overtime_transform_data Quebec false test', {
  
  it('converts Quebec to true', {
    expect_equal(managerial_overtime_transform_data(df)$X1.1Quebec, FALSE)
  })
  
})

