prophetSource('predictors', 'usTaxForesight', 'insuranceArrangement.R')


describe('insurance_arrangement_logic', {

  logic_df <- data.frame('X3.19' = NO,
                       'X3.3' = YES,
                       'X3.3.1' = NO)

    expect_equal(NO_INSURANCE_ARRANGEMENT, insurance_arrangement_logic(logic_df)$prediction)
    expect_equal(1, insurance_arrangement_logic(logic_df)$probability)

  logic_df <- data.frame('X3.19' = YES,
                         'X3.3' = YES,
                         'X3.3.1' = YES)
  
    expect_equal('__noHardcode__', insurance_arrangement_logic(logic_df)$prediction)
    expect_equal(NA, insurance_arrangement_logic(logic_df)$probability)
 
})


