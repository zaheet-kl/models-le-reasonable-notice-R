prophetSource('predictors', 'caTaxForesight', 'tangibleExpenditure.R')
  
# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X2.1'='Chattel (tangible personal property that is not affixed to real property, e.g., furniture, computer, etc.)',
                 'X1.3'=factor('1'),
                 'X2.3'=factor('1'),
                 'X4.1'=factor('1'),
                 'X4.6'=factor('1'),
                 'X4.7'=factor('1'),
                 'X4.8'=factor('1'))
     
describe('tangible_expenditures_transform_data', {
 it('converts type of 1.3 to numeric', {
   expect_equal(is.numeric(tangible_expenditures_transform_data(df)$X1.3), TRUE)
 })
               
  it('converts type of 2.3 to numeric', {
    expect_equal(is.numeric(tangible_expenditures_transform_data(df)$X2.3), TRUE)
  })

  it('converts type of 4.1 to numeric', {
    expect_equal(is.numeric(tangible_expenditures_transform_data(df)$X4.1), TRUE)
  })

  it('converts type of 4.6 to numeric', {
    expect_equal(is.numeric(tangible_expenditures_transform_data(df)$X4.6), TRUE)
  })

  it('converts type of 4.7 to numeric', {
    expect_equal(is.numeric(tangible_expenditures_transform_data(df)$X4.7), TRUE)
  })

  it('converts type of 4.8 to numeric', {
    expect_equal(is.numeric(tangible_expenditures_transform_data(df)$X4.8), TRUE)
  })

  it('converts type of 2.1 to factor', {
    expect_equal(is.factor(tangible_expenditures_transform_data(df)$X2.1), TRUE)
  })

})
