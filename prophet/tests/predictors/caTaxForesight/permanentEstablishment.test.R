prophetSource('predictors', 'caEmploymentForesight', 'reasonableNotice.R')

permanent_establishment_constants <- data.frame(PERMANENT_ESTABLISHMENT='Permanent establishment',
                                                NO_PERMANENT_ESTABLISHMENT='No permanent establishment',
                                                FPOB_PE='FPOB PE',
                                                NO_FPOB_PE='No FPOB PE',
                                                DEPENDENT_AGENT='Dependent Agent',
                                                INDEPENDENT_AGENT='Independent Agent',
                                                DA_PE='DA PE',
                                                NO_DA_PE='No DA PE',
                                                SERVICES_PE='Services PE',
                                                NO_SERVICES_PE='No Services PE',
                                                YES='Yes',
                                                NO='No',
                                                SALE_OR_LEASE_OF_PROPERTY='The sale or lease of personal property, manufactured or purchased, tangible or intangible in Canada',
                                                PERFORMANCE_OF_SERVICES='The performance of services in Canada',
                                                stringsAsFactors=FALSE)

df <- data.frame(X1.4="1",
                 X1.5="1",
                 X1.15="1")

describe('permanent_establishment_transform_data_old', {
  it('converts type of 1.4 to numeric', {
    expect_equal(is.numeric(permanent_establishment_transform_data_old(df)$X1.4), TRUE)
  })
  
  it('converts type of 1.5 to numeric', {
    expect_equal(is.numeric(permanent_establishment_transform_data_old(df)$X1.5), TRUE)
  })
  
  it('converts type of 1.15 to numeric', {
    expect_equal(is.numeric(permanent_establishment_transform_data_old(df)$X1.15), TRUE)
  })
  
  it('log of 1.4', {
    expect_equal(permanent_establishment_transform_data_old(df)$X1.4log, log(1))
  })
  
  it('log of 1.5', {
    expect_equal(permanent_establishment_transform_data_old(df)$X1.5log, log(1))
  })
})

describe('fpob model use', {
  
  df <- data.frame(X1.1=permanent_establishment_constants$YES)
  
  it('test use of model when true', {
    expect_equal(fpob_model_use_old(df), TRUE)
  })
  
  df <- data.frame(X1.1=permanent_establishment_constants$NO)
  
  it('test use of model when false', {
    expect_equal(fpob_model_use_old(df), FALSE)
  })
})

describe('da model use', {
  
  df <- data.frame(X2.1=permanent_establishment_constants$YES,
                   X2.3=permanent_establishment_constants$YES, 
                   X2.11=permanent_establishment_constants$YES)
  
  it('test use of model when true', {
    expect_equal(da_model_use_old(df), TRUE)
  })
  
  df <- data.frame(X2.1=permanent_establishment_constants$YES,
                   X2.3=permanent_establishment_constants$YES, 
                   X2.11=permanent_establishment_constants$NO)
  
  it('test use of model when false', {
    expect_equal(da_model_use_old(df), FALSE)
  })
})

describe('da logic', {
  
  df <- data.frame(X2.1=permanent_establishment_constants$YES,
                   X2.3=permanent_establishment_constants$YES, 
                   X2.11=permanent_establishment_constants$NO)
  
  it('DA_PE hardcode prob', {
    expect_equal(permanent_establishment_da_logic_old(df)$da_prob, 0.95)
  })
  
  it('DA_PE hardcode label', {
    expect_equal(permanent_establishment_da_logic_old(df)$da_label, permanent_establishment_constants$DA_PE)
  })
  
  df <- data.frame(X2.1=permanent_establishment_constants$YES,
                   X2.3=permanent_establishment_constants$YES, 
                   X2.11=permanent_establishment_constants$YES)
  
  it('NO_DA_PE hardcode prob', {
    expect_equal(permanent_establishment_da_logic_old(df)$da_prob, 1)
  })
  
  it('NO_DA_PE hardcode label', {
    expect_equal(permanent_establishment_da_logic_old(df)$da_label, permanent_establishment_constants$NO_DA_PE)
  })
  
})

describe('pe calculate probabilities', {
  
  da_label <- permanent_establishment_constants$DA_PE
  da_prob <- 0.8
  fpob_label <- permanent_establishment_constants$FPOB_PE
  fpob_prob <- 0.6
  
  it('both pe prob', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_prob, 0.8)
  })
  
  it('both pe label', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_label, permanent_establishment_constants$PERMANENT_ESTABLISHMENT)
  })
  
  da_label <- permanent_establishment_constants$DA_PE
  fpob_label <- permanent_establishment_constants$NO_FPOB_PE
  
  it('da pe prob', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_prob, 0.8)
  })
  
  it('da pe label', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_label, permanent_establishment_constants$PERMANENT_ESTABLISHMENT)
  })
  
  da_label <- permanent_establishment_constants$NO_DA_PE
  fpob_label <- permanent_establishment_constants$FPOB_PE
  
  it('fpob pe prob', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_prob, 0.6)
  })
  
  it('fpob pe label', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_label, permanent_establishment_constants$PERMANENT_ESTABLISHMENT)
  })
  
  da_label <- permanent_establishment_constants$NO_DA_PE
  fpob_label <- permanent_establishment_constants$NO_FPOB_PE
  
  it('no pe prob', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_prob, 0.6)
  })
  
  it('no pe label', {
    expect_equal(permanent_establishment_calculate_prediction(da_prob, da_label, fpob_prob, fpob_label)$pred_label, permanent_establishment_constants$NO_PERMANENT_ESTABLISHMENT)
  })

})


