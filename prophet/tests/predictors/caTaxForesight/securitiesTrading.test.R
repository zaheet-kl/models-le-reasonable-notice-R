prophetSource('predictors', 'caTaxForesight', 'directorsLiability.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passed from blue-j-api to prophet as strings
df <- data.frame('X2.7'=1,
                 'X3.5'=1,
                 
                 'X2.4.1.1'=FALSE,
                 'X2.4.1.2'=FALSE,
                 'X2.4.1.3'=FALSE,
                 'X2.4.1.4'=FALSE,
                 'X2.4.1.5'=FALSE,
                 'X2.4.1.6'=FALSE)

describe('securities_trading_transform_data', {
  it('converts type of 2.7 to numeric', {
    expect_equal(is.numeric(securities_trading_transform_data(df)$X2.7), TRUE)
  })
  
  it('converts type of 3.5 to numeric', {
    expect_equal(is.numeric(securities_trading_transform_data(df)$X3.5), TRUE)
  })
  
  it('confirms value of 2.7log', {
    expect_equal(securities_trading_transform_data(df)$X2.7log, log(df$X2.7 + 1))
  })
  
  it('confirms value of 3.5log', {
    expect_equal(securities_trading_transform_data(df)$X3.5log, log(df$X3.5 + 1))
  })
  
  it('confirms value of 2.4.1_any', {
    expect_equal(securities_trading_transform_data(df)$X2.4.1_any, any(TRUE %in% df[securities_trading_checkbox_questions]))
  })

})

logic_df <- data.frame('X3.6'=securities_trading_constants$YES,
                       'X3.6.1'=securities_trading_constants$YES)

describe('securities_trading_logic test 1', {
  
  it('tests hardcode logic', {
    
    expect_equal(securities_trading_constants$ON_ACCOUNT_OF_CAPITAL, securities_trading_logic(logic_df)$prediction)
    expect_equal(1, securities_trading_logic(logic_df)$probability)
    
  })
  
})

logic_df$X3.6 <- securities_trading_constants$YES
logic_df$X3.6.1 <- securities_trading_constants$NO

describe('securities_trading_logic test 2', {
  
  it('tests hardcode logic', {
    
    expect_equal(securities_trading_constants$INCOME_FROM_BUSINSS, securities_trading_logic(logic_df)$prediction)
    expect_equal(1, securities_trading_logic(logic_df)$probability)
    
  })
  
})

logic_df$X3.6 <- securities_trading_constants$NO
logic_df$X3.6.1 <- securities_trading_constants$NO

describe('securities_trading_logic test 3', {
  
  it('tests hardcode logic', {
    
    expect_equal(securities_trading_constants$NO_HARDCODE, securities_trading_logic(logic_df)$prediction)
    expect_equal(NA, securities_trading_logic(logic_df)$probability)
    
  })
  
})