prophetSource('predictors', 'caTaxForesight', 'homeOffice.R')

home_office_constants <- data.frame(NOT_DEDUCTIBLE = 'Not Deductible',
                                    DEDUCTIBLE = 'Deductible',
                                    NOT_USE_OF_OFFICE = 'Not use of office',
                                    USE_OF_OFFICE = 'Use of office',
                                    NOT_PRINCIPAL_PLACE = 'Not principal place',
                                    PRINCIPAL_PLACE = 'Principal place',
                                    EMPLOYEE = 'Employee',
                                    BUSINESS = 'Business (including independent contractor)',
                                    NO_HARDCODE = '__noHardcode__',
                                    stringsAsFactors = FALSE)

df <- data.frame('X1.1' = home_office_constants$EMPLOYEE,
                 'X2.3' = "1",
                 'X2.4' = "1",
                 'X2.5' = "1",
                 'X2.6' = "1",
                 'X2.9' = "1",
                 'X2.10' = "1",
                 'X2.11' = "1",
                 'X2.12' = "1",
                 'X3.3' = "1",
                 'X3.4' = "1",
                 stringsAsFactors = TRUE)

describe('home_office_transform_data', {
  
  it('tests data transform logic and values', {
  
    # check when 1.1 is employee
    df$X1.1 <- home_office_constants$EMPLOYEE
    expect_equal(is.numeric(home_office_transform_data(df)$X2.3), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X2.4), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X2.5), TRUE) 
    expect_equal(is.numeric(home_office_transform_data(df)$X2.6), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X3.3), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X3.4), TRUE)
    expect_equal(home_office_transform_data(df)$X2.4_office, home_office_transform_data(df)$X2.4)
    expect_equal(home_office_transform_data(df)$X2.6_office, home_office_transform_data(df)$X2.6)
    
    # check when 1.1 is business
    df$X1.1 <- home_office_constants$BUSINESS
    expect_equal(is.numeric(home_office_transform_data(df)$X2.9), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X2.10), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X2.11), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X2.12), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X3.3), TRUE)
    expect_equal(is.numeric(home_office_transform_data(df)$X3.4), TRUE)
    expect_equal(home_office_transform_data(df)$X2.4_office, home_office_transform_data(df)$X2.10)
    expect_equal(home_office_transform_data(df)$X2.6_office, home_office_transform_data(df)$X2.12)
    
  })
})

describe('home_office_logic', {
  
  it('tests hardcode logic', {
    
    logic_df <- data.frame('X1.3' = 'No')
    expect_equal(home_office_constants$NOT_DEDUCTIBLE, home_office_logic(logic_df)$prediction)
    expect_equal(1, home_office_logic(logic_df)$probability)
    
    logic_df <- data.frame('X2.1' = 'Yes',
                           'X1.1' = home_office_constants$EMPLOYEE)
    expect_equal(home_office_constants$NOT_DEDUCTIBLE, home_office_logic(logic_df)$prediction)
    expect_equal(1, home_office_logic(logic_df)$probability)
    
    logic_df <- data.frame('X1.4' = 'Own',
                           'X1.5.1' = TRUE,
                           'X1.5.2' = FALSE,
                           'X1.5.3' = FALSE)
    expect_equal(home_office_constants$NOT_DEDUCTIBLE, home_office_logic(logic_df)$prediction)
    expect_equal(1, home_office_logic(logic_df)$probability)
    
    logic_df <- data.frame('X1.1' = home_office_constants$EMPLOYEE,
                           'X1.5.2' = TRUE)
    expect_equal(home_office_constants$NOT_DEDUCTIBLE, home_office_logic(logic_df)$prediction)
    expect_equal(1, home_office_logic(logic_df)$probability)
    
    logic_df <- data.frame('X1.1' = home_office_constants$EMPLOYEE,
                           'X1.5.3' = TRUE)
    expect_equal(home_office_constants$NOT_DEDUCTIBLE, home_office_logic(logic_df)$prediction)
    expect_equal(1, home_office_logic(logic_df)$probability)
    
    logic_df <- data.frame('X5.4' = 'Yes')
    expect_equal(home_office_constants$NOT_DEDUCTIBLE, home_office_logic(logic_df)$prediction)
    expect_equal(1, home_office_logic(logic_df)$probability)
    
    logic_df <- data.frame('X5.4' = 'No')
    expect_equal(home_office_constants$DEDUCTIBLE, home_office_logic(logic_df)$prediction)
    expect_equal(0, home_office_logic(logic_df)$probability)
    
  })
  
})
