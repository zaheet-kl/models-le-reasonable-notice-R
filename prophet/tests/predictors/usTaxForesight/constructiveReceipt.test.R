prophetSource('predictors', 'usTaxForesight', 'constructiveReceipt.R')


describe('constructive receipt logic test', {
  

logic_df <- data.frame('X4.9' = 'No')
  expect_equal(CONSTRUCTIVE_RECEIPT, constructive_receipt_logic(logic_df)$prediction)
  expect_equal(0, constructive_receipt_logic(logic_df)$probability)

logic_df <- data.frame('X4.9' = 'Yes')
  expect_equal('__noHardcode__', constructive_receipt_logic(logic_df)$prediction)
  expect_equal(NA, constructive_receipt_logic(logic_df)$probability)

})
