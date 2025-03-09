prophetSource('predictors', 'usTaxForesight', 'nonresidentTradeOrBusiness.R')

df <- data.frame('X2.1.3'=factor('1'),
                 'X2.1.4'=factor('1'))

describe('nonresident_trade_or_business', {
  
  it('converts type of 2.1.3 to numeric', {
    expect_equal(is.numeric(nonresident_trade_or_business_transform_data(df)$X2.1.3), TRUE)
  })
  
  it('converts type of 2.1.4 to numeric', {
    expect_equal(is.numeric(nonresident_trade_or_business_transform_data(df)$X2.1.4), TRUE)
  })

})


describe('nonresident_trade_or_business logic test', {

  logic_df <- data.frame(X3.1 = YES)
    
    expect_equal(nonresident_trade_or_business_logic(logic_df)$prediction, NOT_ENGAGED)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2 = NONRESIDENT_ALIEN_INDIVIDUAL,
                         X1.2.1 = YES)
  
    expect_equal(nonresident_trade_or_business_logic(logic_df)$prediction, ENGAGED)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2 = NONRESIDENT_ALIEN_INDIVIDUAL,
                        X2.1 = YES,
                        X2.1.1= YES,
                        X2.1.2 = YES,
                        X2.1.3 = 90,
                        X2.1.4 = 3000)
  
    expect_equal(nonresident_trade_or_business_logic(logic_df)$prediction, NOT_ENGAGED)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2 = NONRESIDENT_ALIEN_INDIVIDUAL,
                         X2.1 = YES,
                         X2.1.1 = NO, 
                         X2.1.2 = NO,
                         X2.1.1 = YES, 
                         X2.1.2 = YES,
                         X2.1.3 = 91,
                         X2.1.4 = 3001)
  
    expect_equal(nonresident_trade_or_business_logic(logic_df)$prediction, ENGAGED)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2 = FOREIGN_CORPORATION,
                         X2.1 = YES)
  
    expect_equal(nonresident_trade_or_business_logic(logic_df)$prediction, ENGAGED)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X2.2 = YES,
                         X2.2.1 = NO)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$prediction, NOT_ENGAGED)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X2.3 = YES,
                         X2.3.1 = NO)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$prediction, NOT_ENGAGED)
    expect_equal(nonresident_trade_or_business_logic(logic_df)$probability, 1)

})

