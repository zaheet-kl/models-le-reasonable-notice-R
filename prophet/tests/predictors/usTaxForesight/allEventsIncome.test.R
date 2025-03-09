prophetSource('predictors', 'usTaxForesight', 'allEventsIncome.R')

describe('all events income logic test', {
    
  fixed_right_prob <- 0.75
  
  logic_df <- data.frame('X1.2_2' = NO, 
                         'X1.3' = NO)
    expect_equal(1, all_events_income_logic(logic_df)$probability)
    expect_equal(ALL_EVENTS_NOT_SATISFIED, all_events_income_logic(logic_df, fixed_right_prob)$prediction)
  
  logic_df <- data.frame('X1.2_2' = YES, 
                          'X1.3' = YES)  
  fixed_right_label = NO_FIXED_RIGHT
  
    expect_equal(fixed_right_prob, all_events_income_logic(logic_df, fixed_right_prob, fixed_right_label)$probability)
    expect_equal(ALL_EVENTS_NOT_SATISFIED, all_events_income_logic(logic_df, fixed_right_prob, fixed_right_label)$prediction)
  
  logic_df <- data.frame('X1.2_2' = YES, 
                         'X1.3' = YES)  
  fixed_right_label = FIXED_RIGHT
  
    expect_equal(fixed_right_prob, all_events_income_logic(logic_df, fixed_right_prob, fixed_right_label)$probability)
    expect_equal(ALL_EVENTS_SATISFIED, all_events_income_logic(logic_df, fixed_right_prob, fixed_right_label)$prediction)
    
})