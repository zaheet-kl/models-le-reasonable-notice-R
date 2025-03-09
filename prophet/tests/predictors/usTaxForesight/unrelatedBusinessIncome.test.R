prophetSource('predictors', 'usTaxForesight', 'unrelatedBusinessIncome.R')



describe('unrelated_business_income_logic', {
  
  logic_df <- data.frame(X2.1.1 = YES)
    expect_equal(unrelated_business_income_logic(logic_df)$prediction, UNRELATED_BUSINESS_INCOME)
    expect_equal(unrelated_business_income_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X5.1 = YES,
                         X5.2 = YES)
    expect_equal(unrelated_business_income_logic(logic_df)$prediction, EXCLUDED)
    expect_equal(unrelated_business_income_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X2.1 = NO,
                         X2.2 = NO,
                         X3.2 = YES,
                         X3.3 = YES)
    expect_equal(unrelated_business_income_logic(logic_df)$prediction, NOT_UNRELATED_BUSINESS_INCOME)
    expect_equal(unrelated_business_income_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X2.1 = YES,
                         X2.2 = YES,
                         X3.2 = NO,
                         X3.3 = NO)
    expect_equal(unrelated_business_income_logic(logic_df)$prediction, '__noHardcode__')
    expect_equal(unrelated_business_income_logic(logic_df)$probability, NA)
  
  
})


