prophetSource('predictors', 'usTaxForesight', 'debtVsEquity.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X3.3'=factor('Not applicable'))

describe('debt_vs_equity_transform_data', {
  it('converts 3.3 from N/A to No - the amounts were not proportional', {
    expect_equal(debt_vs_equity_transform_data(df)$X3.3, factor("No - the amounts were not proportional"))
  })
})
