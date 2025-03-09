prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

home_office_principal_employee_model <- readRDS(prophetPath('models', 'caTaxForesight', 'homeOffice', 'ho_emp_v1.rds'))
home_office_principal_business_model <- readRDS(prophetPath('models', 'caTaxForesight', 'homeOffice', 'ho_bus_v1.rds'))
home_office_use_of_office_model <- readRDS(prophetPath('models', 'caTaxForesight', 'homeOffice', 'ho_uoo_v1.rds'))

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


home_office_factors <- function() {
  return(c(
    "1.1",
    "1.3",
    "1.4",
    "1.5.1",
    "1.5.2",
    "1.5.3",
    "2.1",
    "2.2",
    "2.3",
    "2.4",
    "2.5",
    "2.6",
    "2.7",
    "2.8",
    "2.9",
    "2.10",
    "2.11",
    "2.12",
    "3.1",
    "3.2",
    "3.3",
    "3.4",
    "5.4"
  ))
}

home_office_transform_data <- function(df) {
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, home_office_factors, FALSE, answered_questions)

  df$X2.4_office <- ''
  df$X2.6_office <- ''

  if (df$X1.1 == home_office_constants$EMPLOYEE) {

    df <- format_dataframe_numerics(df, c('X2.3', 'X2.4', 'X2.5', 'X2.6', 'X3.3', 'X3.4'))

    df$X2.4_office <- df$X2.4
    df$X2.6_office <- df$X2.6

  } else if (df$X1.1 == home_office_constants$BUSINESS) {

    df <- format_dataframe_numerics(df, c('X2.9', 'X2.10', 'X2.11', 'X2.12', 'X3.3', 'X3.4'))

    df$X2.4_office <- df$X2.10
    df$X2.6_office <- df$X2.12

  }
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, home_office_factors, TRUE, answered_questions)

  return(df)
}

home_office_logic <- function(df) {
  
  # make the NULL values UNANSWERED
  answered_questions <- names(df)
  df <- empty_question_transformation(df, home_office_factors, FALSE, answered_questions)

  if (df$X1.3 == 'No' |
      (df$X2.1 == 'Yes' & df$X1.1 == home_office_constants$EMPLOYEE) |
      (df$X1.4 == 'Own' & df$X1.5.1 == TRUE & df$X1.5.2 == FALSE & df$X1.5.3 == FALSE) |
      (df$X1.1 == home_office_constants$EMPLOYEE & df$X1.5.2 == TRUE) |
      (df$X1.1 == home_office_constants$EMPLOYEE & df$X1.5.3 == TRUE) |
      df$X5.4 == 'Yes') {
    pred_label <- home_office_constants$NOT_DEDUCTIBLE
    pred_prob <- 1
  } else if (df$X5.4 == 'No') {
    pred_label <- home_office_constants$DEDUCTIBLE
    pred_prob <- 0
  } else {
    pred_label <- home_office_constants$NO_HARDCODE
    pred_prob <- NA
  }

  return(list(
    prediction = pred_label,
    probability = pred_prob
  ))

}

home_office_predict <- function(df) {

  df <- home_office_transform_data(df)

  hardcodes <- home_office_logic(df)

  if (hardcodes$prediction == home_office_constants$NOT_DEDUCTIBLE | hardcodes$prediction == home_office_constants$DEDUCTIBLE) {

    # main results
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction

    # empty subresults
    principal_label <- ''
    principal_prob <- ''
    use_of_office_label <- ''
    use_of_office_prob <- ''

  } else {

    # employee model or business model
    if (df$X1.1 == home_office_constants$EMPLOYEE) {

      principal_prob <- predict(home_office_principal_employee_model, df, type = 'response')
      principal_label <- ifelse(principal_prob > 0.5, home_office_constants$NOT_PRINCIPAL_PLACE, home_office_constants$PRINCIPAL_PLACE)

    } else if (df$X1.1 == home_office_constants$BUSINESS) {

      principal_prob <- predict(home_office_principal_business_model, df, type = 'response')
      principal_label <- ifelse(principal_prob > 0.5, home_office_constants$NOT_PRINCIPAL_PLACE, home_office_constants$PRINCIPAL_PLACE)

    }

    # use of office model
    use_of_office_prob <- predict(home_office_use_of_office_model, df, type = 'response')
    use_of_office_label <- ifelse(use_of_office_prob > 0.5,  home_office_constants$NOT_USE_OF_OFFICE, home_office_constants$USE_OF_OFFICE)

    log$info('here', list(use_of_office_label=use_of_office_label, use_of_office_prob=use_of_office_prob, df=df))
    # if both are not deductible return average probability
    if (principal_label == home_office_constants$NOT_PRINCIPAL_PLACE & use_of_office_label ==  home_office_constants$NOT_USE_OF_OFFICE) {

      pred_prob <- (principal_prob + use_of_office_prob) / 2
      pred_label <- home_office_constants$NOT_DEDUCTIBLE

      # if both are deductible return the max probability (min since deductible < 0.5)
    } else if (principal_label == home_office_constants$PRINCIPAL_PLACE & use_of_office_label == home_office_constants$USE_OF_OFFICE) {

      pred_prob <- min(principal_prob, use_of_office_prob)
      pred_label <- home_office_constants$DEDUCTIBLE

      # if one is deductible return results from that one
    } else if (principal_label == home_office_constants$PRINCIPAL_PLACE & use_of_office_label ==  home_office_constants$NOT_USE_OF_OFFICE) {

      pred_prob <- principal_prob
      pred_label <- home_office_constants$DEDUCTIBLE

    } else if (principal_label == home_office_constants$NOT_PRINCIPAL_PLACE & use_of_office_label == home_office_constants$USE_OF_OFFICE) {

      pred_prob <- use_of_office_prob
      pred_label <- home_office_constants$DEDUCTIBLE
    }

  }

  return(list(
    prediction = pred_label,
    probability = pred_prob,
    subResults = list(
      list(
        group = 'principalPlace',
        prediction = principal_label,
        probability=principal_prob
      ),
      list(
        group = 'useOfOffice',
        prediction = use_of_office_label,
        probability=use_of_office_prob
      )
    )
  ))

}

home_office_interim_predict <- function(df){
  
  df <- home_office_transform_data(df)
  
  hardcodes <- home_office_logic(df)
  
  if (hardcodes$prediction == home_office_constants$NOT_DEDUCTIBLE | hardcodes$prediction == home_office_constants$DEDUCTIBLE) {
    
    # main results
    pred_prob <- hardcodes$probability
    pred_label <- hardcodes$prediction
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
    
  } else {
    
    # employee model or business model
    if (df$X1.1 == home_office_constants$EMPLOYEE) {
      
      principal_employee_interim <- create_interim_predictor(home_office_principal_employee_model,
                                                             home_office_constants$PRINCIPAL_PLACE,
                                                             home_office_constants$NOT_PRINCIPAL_PLACE,
                                                             data.frame(),
                                                             1)(df)
      
      principal_prob <- principal_employee_interim$probability
      principal_label <- principal_employee_interim$prediction
      
    } else if (df$X1.1 == home_office_constants$BUSINESS) {
      
      principal_business_interim <- create_interim_predictor(home_office_principal_business_model,
                                                             home_office_constants$PRINCIPAL_PLACE,
                                                             home_office_constants$NOT_PRINCIPAL_PLACE,
                                                             data.frame(),
                                                             1)(df)
      
      principal_prob <- principal_business_interim$probability
      principal_label <- principal_business_interim$prediction
      
    }
    
    
    # use of office interim is always 50 (its the last page and never seen in interim)
    use_of_office_prob <- 0.5
    use_of_office_label <- home_office_constants$USE_OF_OFFICE
    
    # if both are not deductible return average probability
    if (principal_label == home_office_constants$NOT_PRINCIPAL_PLACE & use_of_office_label ==  home_office_constants$NOT_USE_OF_OFFICE) {
      
      pred_prob <- (principal_prob + use_of_office_prob) / 2
      pred_label <- home_office_constants$NOT_DEDUCTIBLE
      
      # if both are deductible return the max probability (min since deductible < 0.5)
    } else if (principal_label == home_office_constants$PRINCIPAL_PLACE & use_of_office_label == home_office_constants$USE_OF_OFFICE) {
      
      pred_prob <- min(principal_prob, use_of_office_prob)
      pred_label <- home_office_constants$DEDUCTIBLE
      
      # if one is deductible return results from that one
    } else if (principal_label == home_office_constants$PRINCIPAL_PLACE & use_of_office_label ==  home_office_constants$NOT_USE_OF_OFFICE) {
      
      pred_prob <- principal_prob
      pred_label <- home_office_constants$DEDUCTIBLE
      
    } else if (principal_label == home_office_constants$NOT_PRINCIPAL_PLACE & use_of_office_label == home_office_constants$USE_OF_OFFICE) {
      
      pred_prob <- use_of_office_prob
      pred_label <- home_office_constants$DEDUCTIBLE
    }
    
    return(list(
      prediction=pred_label,
      probability=pred_prob
    ))
    
  }
  
  
  
}



