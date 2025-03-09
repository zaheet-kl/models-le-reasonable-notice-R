prophetSource('predictors', 'usTaxForesight', 'tenThirtyOne.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X4.1'=factor('1'))

describe('ten_thirty_one_transform_data', {
  it('converts type of 4.1 to numeric', {
    expect_equal(is.numeric(ten_thirty_one_transform_data(df)$X4.1), TRUE)
  })
  
})

describe('ten_thirty_one_logic', {

  logic_df <- data.frame(X3.1.01_customChoiceValue = 'inventory')
    expect_equal(ten_thirty_one_logic(logic_df)$probability, 1)
    expect_equal(ten_thirty_one_logic(logic_df)$prediction, NOT_HPI)
            
  logic_df <- data.frame(X2.1.02 = 'Yes',
                         X2.1.03 = 'Yes',
                         X2.1.04 = 'Yes')
    expect_equal(ten_thirty_one_logic(logic_df)$probability, 0)
    expect_equal(ten_thirty_one_logic(logic_df)$prediction, HPI)
  
  logic_df <- data.frame(X2.1.02 = 'No',
                         X2.1.03 = 'No',
                         X2.1.04 = 'No')
    expect_equal(ten_thirty_one_logic(logic_df)$probability, NA)
    expect_equal(ten_thirty_one_logic(logic_df)$prediction, '__noHardcode__')


})
