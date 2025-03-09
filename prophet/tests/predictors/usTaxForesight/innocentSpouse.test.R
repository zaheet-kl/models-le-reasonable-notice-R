prophetSource('predictors', 'usTaxForesight', 'innocentSpouse.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X2.1'=1,
                 'X2.3'=1,
                 'X2.8'=1,
                 'X2.8.01'=1,
                 'X2.10'=1,
                 'X2.11'=1,
                 'X3.2'=1,
                 'X4.4'=1,
                 'X1.1_customChoiceValue'=factor('District of Columbia Circuit'),
                 'X1.4_customChoiceValue'=factor('Divorced'),
                 'X2.2_customChoiceValue'=factor('High School'),
                 'X2.4_customChoiceValue'=factor('The person reviewed the tax return before signing or submitting electronically'),
                 'X2.5'=factor('No'),
                 'X2.6_customChoiceValue'=factor('The person filled out the tax return '),
                 'X2.7'=factor('No'),
                 'X2.9'=factor('No'),
                 'X3.1_customChoiceValue'=factor('The income tax liability arose from income that both spouses earned, or property that both spouses owned'),
                 'X3.3'=factor('No'),
                 'X3.4'=factor('No'),
                 'X3.5'=factor('No'),
                 'X4.1'=factor('No'),
                 'X4.2'=factor('No'),
                 'X4.3'=factor('No'),
                 'X4.5'=factor('No'))

describe('innocent_spouse_transform_data', {
  it('converts type of 2.1 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X2.1), TRUE)
  })
  
  it('converts type of 2.3 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X2.3), TRUE)
  })
  
  it('converts type of 2.8 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X2.8), TRUE)
  })
  
  it('converts type of 2.8.01 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X2.8.01), TRUE)
  })
  
  it('converts type of 2.10 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X2.10), TRUE)
  })
  
  it('converts type of 2.11 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X2.11), TRUE)
  })
  
  it('converts type of 3.2 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X3.2), TRUE)
  })
  
  it('converts type of 4.4 to numeric', {
    expect_equal(is.numeric(innocent_spouse_transform_data(df)$X4.4), TRUE)
  })
  
  
  it('converts type of 1.1_ccv to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X1.1_customChoiceValue), TRUE)
  })
  
  it('converts type of 1.4_ccv to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X1.4_customChoiceValue), TRUE)
  })
  
  it('converts type of 2.2_ccv to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X2.2_customChoiceValue), TRUE)
  })
  
  it('converts type of 2.4_ccv to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X2.4_customChoiceValue), TRUE)
  })
  
  it('converts type of 2.5 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X2.5), TRUE)
  })
  
  it('converts type of 2.6_ccv to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X2.6_customChoiceValue), TRUE)
  })
  
  it('converts type of 2.7 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X2.7), TRUE)
  })
  
  it('converts type of 2.9 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X2.9), TRUE)
  })
  
  it('converts type of 3.1_ccv to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X3.1_customChoiceValue), TRUE)
  })
  
  it('converts type of 3.3 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X3.3), TRUE)
  })
  
  it('converts type of 3.4 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X3.4), TRUE)
  })
  
  it('converts type of 3.5 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X3.5), TRUE)
  })
  
  it('converts type of 4.1 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X4.1), TRUE)
  })
  
  it('converts type of 4.2 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X4.2), TRUE)
  })
  
  it('converts type of 4.3 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X4.3), TRUE)
  })
  
  it('converts type of 4.5 to factor', {
    expect_equal(is.factor(innocent_spouse_transform_data(df)$X4.5), TRUE)
  })

})


fairness_prob = 0.6
knowledge_prob = 0.8

fairness_prob_above_50 <- convert_probability_above_50(fairness_prob)
knowledge_prob_above_50 <- convert_probability_above_50(knowledge_prob)

outcome <- list(pred_label=NOT_A_DE_FACTO_PARTNERSHIP, pred_prob=0.67)


df <- data.frame(X1.2 = YES_PURSUANT_TO_A_AND_B,
                 X1.3.03 = ERRONEOUS_ITEM_BELONGING_TO_SPOUSE,
                 X1.3.05 = YES,
                 X2.1 = 0)

fairness_outcome = LIABILITY_IS_UNFAIR
knowledge_outcome = NO_CONSTRUCTIVE_KNOWLEDGE

describe('innocent_spouse_relief test', {
  
  it('tests innocent spouse relief and logic function calls', {
    expect_equal(innocent_spouse_relief(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=INNOCENT_SPOUSE_RELIEF,
                      pred_prob=(fairness_prob_above_50 + knowledge_prob_above_50)/2))
    
  })
})

df <- data.frame(X1.2 = YES_PURSUANT_TO_C_AND_A,
                 X1.3.06 = YES,
                 X2.1 = 0)

knowledge_outcome = NO_CONSTRUCTIVE_KNOWLEDGE
fairness_outcome = LIABILITY_IS_UNFAIR

describe('traditional_relief test', {
  
  it('tests traditional relief and logic function calls', {
    expect_equal(traditional_relief(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=TRADITIONAL_RELIEF,
                      pred_prob=(fairness_prob_above_50 + knowledge_prob_above_50)/2))
  })
})
  

df <- data.frame(X1.2 = YES_PURSUANT_TO_F,
                 X1.3.01 = UNDERPAYMENT_OF_TAX,
                 X1.3.05 = NO)

fairness_outcome = LIABILITY_IS_UNFAIR

describe('equitable_relief_no_sl_overlap test', {
  
  it('tests equitable relief no sl overlap and logic function calls', {
    expect_equal(equitable_relief_no_sl_overlap(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=EQUITABLE_RELIEF,
                      pred_prob=fairness_prob_above_50))
  })
})
  

df <- data.frame(X1.2 = YES_PURSUANT_TO_C4,
                 X1.3 = NO,
                 X1.3.02 = NO,
                 X1.3.04 = NONE_OF_THE_ABOVE,
                 X1.3.06 = NO)

fairness_outcome = LIABILITY_IS_UNFAIR

describe('equitable_relief_66 test', {
  
  it('tests equitable relief 66 and logic function calls', {
    expect_equal(equitable_relief_66(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=EQUITABLE_RELIEF,
                      pred_prob=fairness_prob_above_50))
  })
})

df <- data.frame(X1.3.03 = ERRONEOUS_ITEM_BELONGING_TO_PERSON,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_SOLELY_OWNED,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_OWNED_BY_BOTH)

fairness_outcome = LIABILITY_IS_UNFAIR

describe('equitable_relief_joint_return test', {
  
  it('tests equitable relief joint return and logic function calls', {
    expect_equal(equitable_relief_joint_return(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=EQUITABLE_RELIEF,
                      pred_prob=fairness_prob_above_50))
  })
})


df <- data.frame(X1.2 = YES_PURSUANT_TO_C_AND_D,
                 X1.3.05 = YES,
                 X1.4_customChoiceValue = DIVORCED,
                 X2.1 = 0,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_FROM_SPOUSE,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_ATTRIBUTABLE_THROUGH_LAW,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_CONTROLLED_BY_SPOUSE)

describe('separation_of_liability_knowledge test', {
  
  it('tests separation of liability knowledge and logic function calls', {
    expect_equal(separation_of_liability_knowledge(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=SEPARATION_OF_LIABILITY,
                      pred_prob=1))
  })
})


df <- data.frame(X1.2 = YES_PURSUANT_TO_C_AND_D,
                 X1.2 = YES_PURSUANT_TO_A_AND_B,
                 X1.3.05 = YES,
                 X1.4_customChoiceValue = DIVORCED,
                 X2.1 = 1,
                 X4.5 = YES,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_FROM_SPOUSE,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_ATTRIBUTABLE_THROUGH_LAW,
                 X3.1_customChoiceValue = INCOME_OR_PROPERTY_CONTROLLED_BY_SPOUSE)

describe('separation_of_liability_abuse test', {
  
  it('tests separation of liability abuse and logic function calls', {
    expect_equal(separation_of_liability_abuse(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=SEPARATION_OF_LIABILITY,
                      pred_prob=1))
  })
})

df <- data.frame(X1.1 = 0)
fairness_outcome = LIABILITY_IS_FAIR

describe('no_relief test', {
  
  it('tests no relief and logic function calls', {
    expect_equal(no_relief(df, fairness_outcome, knowledge_outcome), TRUE)
    expect_equal(innocent_spouse_logic(df, fairness_outcome, knowledge_outcome, fairness_prob, knowledge_prob),
                 list(pred_label=NO_RELIEF,
                      pred_prob=fairness_prob_above_50))
  })
})
