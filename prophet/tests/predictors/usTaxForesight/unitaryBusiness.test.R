prophetSource('predictors', 'usTaxForesight', 'unitaryBusiness.R')

describe('unitary_business_logic', {
  
  logic_df <- data.frame(X1.1 = unitary_business_constants$STATE_NOT_LISTED_ABOVE)
  expect_equal(unitary_business_logic(logic_df)$prediction, unitary_business_constants$SR)
  expect_equal(unitary_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2ak = unitary_business_constants$NO)
  expect_equal(unitary_business_logic(logic_df)$prediction, unitary_business_constants$NO_CG)
  expect_equal(unitary_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2co.2 = unitary_business_constants$YES)
  expect_equal(unitary_business_logic(logic_df)$prediction, unitary_business_constants$UB)
  expect_equal(unitary_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2tx = unitary_business_constants$YES)
  expect_equal(unitary_business_logic(logic_df)$prediction, unitary_business_constants$UB)
  expect_equal(unitary_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.3 = unitary_business_constants$YES)
  expect_equal(unitary_business_logic(logic_df)$prediction, unitary_business_constants$UB)
  expect_equal(unitary_business_logic(logic_df)$probability, 1)
  
  logic_df <- data.frame(X1.2co.2 = unitary_business_constants$NO)
  expect_equal(unitary_business_logic(logic_df)$prediction, unitary_business_constants$NO_UB)
  expect_equal(unitary_business_logic(logic_df)$probability, 1)
  
})

describe('unitary_business_transform_data', {
  
  df <- data.frame(X1.1=as.factor('Arizona'), X3.4=as.factor('Yes'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.arizona, as.factor('true'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.not_arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.not_arizona, as.factor('false'))
  
  df <- data.frame(X1.1=as.factor('Arizona'), X3.4=as.factor('No'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.arizona, as.factor('true'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.not_arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.not_arizona, as.factor('false'))
  
  df <- data.frame(X1.1=as.factor('Ohio'), X3.4=as.factor('Yes'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.not_arizona, as.factor('true'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.not_arizona, as.factor('false'))
  
  df <- data.frame(X1.1=as.factor('Ohio'), X3.4=as.factor('No'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4Yes.not_arizona, as.factor('false'))
  expect_equal(unitary_business_transform_data(df)$X3.4No.not_arizona, as.factor('true'))
  
})








