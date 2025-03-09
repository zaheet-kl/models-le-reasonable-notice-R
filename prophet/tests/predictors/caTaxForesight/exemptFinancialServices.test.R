prophetSource('predictors', 'caTaxForesight', 'exemptFinancialServices.R')


logic_df <- data.frame('X1.2' = 'Not None of the Above')
  
describe('exempt_financial_services_logic test 1', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ARRANGING_FS, exempt_financial_services_logic(logic_df)$prediction)
    expect_equal(1, exempt_financial_services_logic(logic_df)$probability)
    
  })
  
})
  
logic_df <- data.frame('X1.2' = NONE_OF_THE_ABOVE_EFS)

describe('exempt_financial_services_logic test 1', {
  
  it('tests hardcode logic', {
    
    expect_equal(NO_HARDCODE, exempt_financial_services_logic(logic_df)$prediction)
    expect_equal(1, exempt_financial_services_logic(logic_df)$probability)
    
  })
  
})
