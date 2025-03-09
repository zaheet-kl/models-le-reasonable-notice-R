prophetSource('predictors', 'usTaxForesight', 'allEventsExpense.R')


describe('all events expense logic test', {
  
  liability_prob <- 0.1
  
  logic_df <- data.frame('X6.5' = NO,
                         'X1.2_customChoiceValue' = NO,
                         'X2.2' = NO)
  
    expect_equal(1, all_events_expense_logic(logic_df)$probability)
    expect_equal(ALL_EVENTS_NOT_SATISFIED, all_events_expense_logic(logic_df)$prediction)
                 
  logic_df <- data.frame('X6.5' = YES,
                         'X1.2_customChoiceValue' = YES,
                         'X2.2' = YES)
  
  liability_pred = LIABILITY_SATISIFIED
    
    expect_equal(liability_prob, all_events_expense_logic(logic_df, liability_prob, liability_pred)$probability)
    expect_equal(ALL_EVENTS_SATISFIED, all_events_expense_logic(logic_df, liability_prob, liability_pred)$prediction)
                 
  logic_df <- data.frame('X6.5' = YES)
  
    expect_equal(liability_prob, all_events_expense_logic(logic_df, liability_prob, liability_pred)$probability)
    expect_equal(ALL_EVENTS_NOT_SATISFIED, all_events_expense_logic(logic_df, liability_prob, liability_pred)$prediction)
  
})