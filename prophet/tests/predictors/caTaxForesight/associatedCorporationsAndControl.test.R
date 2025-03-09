prophetSource('predictors', 'caTaxForesight', 'associatedCorporationsAndControl.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X1.1_customChoiceValue'=factor('associated'),
                 'X1.3_customChoiceValue'=factor('third_person'),
                 'X1.3.01'=factor('No, continue analysis of de jure control and deemed control'),
                 'X2.1_customChoiceValue'=factor('Not more'),
                 'X2.2_customChoiceValue'=factor('Does not owe debt'),
                 'X2.8_customChoiceValue'=factor('Family'),
                 'X2.9_customChoiceValue'=factor('Shared directors'))

describe('associated_corporations_and_control_transform_data', {
  it('converts type of 1.1_ccv to factor', {
    expect_equal(is.factor(associated_corporations_and_control_transform_data(df)$X1.1_customChoiceValue), TRUE)
  })
  
  it('converts type of 1.3_ccv to factor', {
    expect_equal(is.factor(associated_corporations_and_control_transform_data(df)$X1.3_customChoiceValue), TRUE)
  })
  
  it('converts type of 1.3.01 to factor', {
    expect_equal(is.factor(associated_corporations_and_control_transform_data(df)$X1.3.01), TRUE)
  })
  
  it('converts type of 2.1_ccv to factor', {
    expect_equal(is.factor(associated_corporations_and_control_transform_data(df)$X2.1_customChoiceValue), TRUE)
  })
  
  it('converts type of 2.2_ccv to factor', {
    expect_equal(is.factor(associated_corporations_and_control_transform_data(df)$X2.2_customChoiceValue), TRUE)
  })
  
  it('converts type of 2.8_ccv to factor', {
    expect_equal(is.factor(associated_corporations_and_control_transform_data(df)$X2.8_customChoiceValue), TRUE)
  })
  
  it('converts type of 2.9_ccv to factor', {
    expect_equal(is.factor(associated_corporations_and_control_transform_data(df)$X2.9_customChoiceValue), TRUE)
  })
  
})


ml_prob <- 0.77

logic_df <- data.frame(`X3.1` = FALSE,
                       `X1.1A` = YES,
                       `X1.1A.02` = YES,
                       `X1.1A.1` = YES,
                       `X1.1A.2` = YES)

describe('associated_corporations_and_control_logic test 1', {
  
  it('tests hardcode logic', {
    
    expect_equal(CONTROLLED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = YES,
                       `X1.1A.02` = YES,
                       `X1.1A.1` = YES,
                       `X1.1A.2` = YES)


describe('associated_corporations_and_control_logic test 2', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_CONTROLLED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = YES,
                       `X1.1A.02` = YES,
                       `X1.1A.1` = YES,
                       `X1.1A.2` = YES)

describe('associated_corporations_and_control_logic test 3', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_CONTROLLED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.2` = YES)


describe('associated_corporations_and_control_logic test 4', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.2.01` = YES)


describe('associated_corporations_and_control_logic test 5', {
  
  it('tests hardcode logic', {
    
    expect_equal(ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})


logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.2.1` = NOPE,
                       `X1.2.2` = YES)


describe('associated_corporations_and_control_logic test 6', {
  
  it('tests hardcode logic', {
    
    expect_equal(ASSOCIATED_BUT_125, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.2.2` = NOPE)


describe('associated_corporations_and_control_logic test 7', {
  
  it('tests hardcode logic', {
    
    expect_equal(ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.3_customChoiceValue` = 'does_not_resemble')


describe('associated_corporations_and_control_logic test 8', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})


logic_df <- data.frame(`X3.1` = FALSE,
                       `X1.3A` = YES,
                       `X1.3A.02` = YES,
                       `X1.3A.1` = YES,
                       `X1.3A.2` = YES,
                       `X1.3B` = YES,
                       `X1.3B.02` = YES,
                       `X1.3B.1` = YES,
                       `X1.3B.2` = YES)

describe('associated_corporations_and_control_logic test 9', {
  
  it('tests hardcode logic', {
    
    expect_equal(ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})


logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.3A` = YES,
                       `X1.3A.02` = YES,
                       `X1.3A.1` = YES,
                       `X1.3A.2` = YES,
                       `X1.3B` = YES,
                       `X1.3B.02` = YES,
                       `X1.3B.1` = YES,
                       `X1.3B.2` = YES)

describe('associated_corporations_and_control_logic test 10', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.3C_customChoiceValue` = 'none')

describe('associated_corporations_and_control_logic test 11', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})


logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.4A` = YES,
                       `X1.4A.02` = YES,
                       `X1.4B` = YES,
                       `X1.4B.02` = YES,
                       `X1.4C` = YES,
                       `X1.4C.02` = YES)

describe('associated_corporations_and_control_logic test 12', {
  
  it('tests hardcode logic', {
    
    expect_equal(ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = TRUE,
                       `X1.1A` = NO,
                       `X1.4A` = NOPE,
                       `X1.4A.01` = 'none',
                       `X1.4A.02` = NOPE,
                       `X1.4B` = NOPE,
                       `X1.4B.01` = 'none',
                       `X1.4B.02` = NOPE,
                       `X1.4C` = NOPE,
                       `X1.4C.01` = 'none',
                       `X1.4C.02` = NOPE)

describe('associated_corporations_and_control_logic test 13', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ASSOCIATED, associated_corporations_and_control_logic(logic_df)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df)$probability)
    
  })
  
})

logic_df <- data.frame(`X1.1_customChoiceValue` = 'controls',
                       `X3.1` = FALSE) 

describe('associated_corporations_and_control_logic test 14', {
  
  it('tests hardcode logic', {
    
    expect_equal(CONTROLLED, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = ASSOCIATED)$prediction)
    expect_equal(ml_prob, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = ASSOCIATED)$probability)
    
  })
  
})

logic_df <- data.frame(`X1.1_customChoiceValue` = 'controls', 
                       `X3.1` = TRUE)

describe('associated_corporations_and_control_logic test 15', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_CONTROLLED, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NA)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NA)$probability)
    
  })
  
})

logic_df <- data.frame(`X3.1` = FALSE,
                       `X1.1A` = NO,
                       `X1.1_customChoiceValue` = 'controls') 

describe('associated_corporations_and_control_logic test 16', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_CONTROLLED, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NOT_ASSOCIATED)$prediction)
    expect_equal(ml_prob, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NOT_ASSOCIATED)$probability)
    
  })
  
})

logic_df <- data.frame(`X1.1_customChoiceValue` = 'associated', 
                       `X3.1` = FALSE)

describe('associated_corporations_and_control_logic test 17', {
  
  it('tests hardcode logic', {
    
    expect_equal(ASSOCIATED, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = ASSOCIATED)$prediction)
    expect_equal(ml_prob, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = ASSOCIATED)$probability)
    
  })
  
})

logic_df <- data.frame(`X1.1_customChoiceValue` = 'associated', 
                       `X3.1` = TRUE)

describe('associated_corporations_and_control_logic test 18', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ASSOCIATED, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NA)$prediction)
    expect_equal(1, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NA)$probability)
    
  })
  
})


logic_df <- data.frame(`X3.1` = TRUE,
                       `X2.45` = YES)

describe('associated_corporations_and_control_logic test 19', {
  
  it('tests hardcode logic', {
    
    expect_equal(NOT_ASSOCIATED, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NA)$prediction)
    expect_equal(ml_prob, associated_corporations_and_control_logic(logic_df, ml_prob = 0.77, classification = NA)$probability)
    
  })
  
})
