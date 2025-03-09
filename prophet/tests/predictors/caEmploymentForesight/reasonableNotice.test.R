prophetSource('predictors', 'caEmploymentForesight', 'reasonableNotice.R')

# test 
df <- data.frame("X1.1"=as.factor("Newfoundland"),
                 "X1.2"=as.factor("No"),
                 "X1.3"=as.factor("Employee"),
                 "X2.1.1_customChoiceValue"=as.factor("Professional"),
                 "X2.2"="36",
                 "X2.3"="0.5",
                 "X2.4"="5",
                 "X2.5"="2",
                 "X2.8"=as.factor("No"),
                 "X3.2"=as.factor("No"),
                 "X4.1"=as.factor("Yes"),
                 "X4.1.1"="1",
                 "X4.2"=as.factor("No"),
                 "X5.1"="39200",
                 "X5.2"=as.factor("No"),
                 "X5.3"=as.factor("No"),
                 "X5.4"=as.factor("Yes"),
                 "X6.1"=as.factor("No"),
                 "X6.2"=as.factor("No"),
                 "X6.3"=as.factor("No"),
                 stringsAsFactors=TRUE)

describe('reasonable_notice_transform_data', {
  it('converts type of 2.2 to numeric', {
    expect_equal(is.numeric(reasonable_notice_transform_data(df)$X2.2), TRUE)
  })
  
  it('converts type of 2.3 to numeric', {
    expect_equal(is.numeric(reasonable_notice_transform_data(df)$X2.3), TRUE)
  })
  
  it('converts type of 2.4 to numeric', {
    expect_equal(is.numeric(reasonable_notice_transform_data(df)$X2.4), TRUE)
  })
  
  it('converts type of 2.5 to numeric', {
    expect_equal(is.numeric(reasonable_notice_transform_data(df)$X2.5), TRUE)
  })
  
  it('converts type of 4.1.1 to numeric', {
    expect_equal(is.numeric(reasonable_notice_transform_data(df)$X4.1.1), TRUE)
  })
  
  it('converts type of 5.1 to numeric', {
    expect_equal(is.numeric(reasonable_notice_transform_data(df)$X5.1), TRUE)
  })
  
  it('converts 1.2 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X1.2, 1)
  })
  
  it('converts 2.8 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X2.8, 1)
  })
  
  it('converts 3.2 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X3.2, 1)
  })
  
  it('converts 4.1 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X4.1, 2)
  })
  
  it('converts 4.2 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X4.2, 1)
  })
  
  it('converts 5.2 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X5.2, 1)
  })
  
  it('converts 5.3 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X5.3, 1)
  })
  
  it('converts 5.4 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X5.4, 2)
  })
  
  it('converts 6.1 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X6.1, 1)
  })
  
  it('converts 6.2 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X6.2, 1)
  })
  
  it('converts 6.3 to numeric', {
    expect_equal(reasonable_notice_transform_data(df)$X6.3, 1)
  })
  
})

# test prediction and anchor df
df <- data.frame("X1.1"=as.factor("Alberta"),
                 "X1.2"=as.factor("No"),
                 "X1.3"=as.factor("Employee"),
                 "X2.1.1_customChoiceValue"=as.factor("Professional"),
                 "X2.2"="36",
                 "X2.3"="0.5",
                 "X2.4"="5",
                 "X2.5"="2",
                 "X2.8"=as.factor("No"),
                 "X3.2"=as.factor("No"),
                 "X4.1"=as.factor("Yes"),
                 "X4.1.1"="1",
                 "X4.2"=as.factor("No"),
                 "X5.1"="39200",
                 "X5.2"=as.factor("No"),
                 "X5.3"=as.factor("No"),
                 "X5.4"=as.factor("Yes"),
                 "X6.1"=as.factor("No"),
                 "X6.2"=as.factor("No"),
                 "X6.3"=as.factor("No"),
                 stringsAsFactors=TRUE)

pred_full <- 6.809798
pred_anchor <- 2.38299

describe('reasonable_notice_transform_predict testing', {
  it('output for predict df is 2.38299', {
    expect_equal(abs(reasonable_notice_predict(df)$predictedNumber - pred_anchor) < 0.01, TRUE)
  })
})

df_transformed <- reasonable_notice_transform_data(df)

describe('reasonable_notice_anchor_prediction testing', {
  it('output for anchor is 2.38299', {
    expect_equal(abs(reasonable_notice_anchor_prediction(pred_full, df_transformed)$pred - pred_anchor) < 0.01, TRUE)
  })
})


describe('test combinations for anchoring the prediction', {
  
  df <- data.frame("X2.1.1_customChoiceValue"=as.factor("Upper management"),
                   "X2.3"=1,
                   "X2.8"=2,
                   "X4.1"=2,
                   "X4.1.1"=6)
  
  pred_full <- 100
  
  it('pred_full - 3', {
    expect_equal(reasonable_notice_anchor_prediction(pred_full, df)$pred, pred_full - 3)
  })
  
  df$X2.3 <- 2
  
  it('pred_full - 2', {
    expect_equal(reasonable_notice_anchor_prediction(pred_full, df)$pred, pred_full - 2)
  })
  
  df$X2.3 <- 5
  
  it('pred_full - 1', {
    expect_equal(reasonable_notice_anchor_prediction(pred_full, df)$pred, pred_full - 1)
  })
  
  df$X2.3 <- 6
  
  it('pred_full', {
    expect_equal(reasonable_notice_anchor_prediction(pred_full, df)$pred, pred_full)
  })
  
  df <- data.frame("X2.1.1_customChoiceValue"=as.factor("Clerical"),
                   "X2.3"=1,
                   "X2.8"=1,
                   "X4.1"=1,
                   "X4.1.1"=1)
  
  it('13.87 anchor', {
    expect_equal(reasonable_notice_anchor_prediction(pred_full, df)$pred, 13.87)
  })
  
})



