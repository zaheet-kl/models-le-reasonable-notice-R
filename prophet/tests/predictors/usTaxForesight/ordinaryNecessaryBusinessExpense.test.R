prophetSource('predictors', 'usTaxForesight', 'ordinaryNecessaryBusinessExpense.R')


describe('ordinary_necessary_logic', {

  logic_df <- data.frame(X1.3 = YES, 
                         X1.3.1 = NO, 
                         X3.1_customChoiceValue = NOT_ON, 
                         X3.2 = NO, 
                         X3.3 = NO, 
                         X3.4 = NO, 
                         X3.5 = NO, 
                         X4.1 = NO, 
                         X4.2 = NO, 
                         X4.3 = NO, 
                         X4.4 = NO, 
                         X4.5 = NO, 
                         X4.6 = NOT_APPLICABLE)
    expect_equal(ordinary_necessary_logic(logic_df)$probability, 1)
    expect_equal(ordinary_necessary_logic(logic_df)$prediction, NO_DONBE)
  
    
  logic_df <- data.frame(X1.3 = NO)   
  
    expect_equal(ordinary_necessary_logic(logic_df, sub_prob=0.1, sub_label=CAPITAL_DONBE)$probability, 0.1)
    expect_equal(ordinary_necessary_logic(logic_df, sub_prob=0.1, sub_label=CAPITAL_DONBE)$prediction, NO_DONBE)
  
  logic_df <- data.frame(X1.3 = NO, 
                           X1.3.1 = YES, 
                           X3.1_customChoiceValue = NO)
    expect_equal(ordinary_necessary_logic(logic_df, sub_prob=0.1, sub_label=DONBE, main_prob=0.3, main_label=DONBE)$probability, (convert_probability_above_50(0.1) + convert_probability_above_50(0.3))/2)
    expect_equal(ordinary_necessary_logic(logic_df, sub_prob=0.1, sub_label=DONBE, main_prob=0.3, main_label=DONBE)$prediction, DONBE)

})