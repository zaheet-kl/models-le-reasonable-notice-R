prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

food_liability_model <- readRDS(prophetPath('models', 'ukTaxForesight', 'foodLiability','food.rds'))
food_liability_snack_model <- readRDS(prophetPath('models', 'ukTaxForesight', 'foodLiability','snacks.rds'))
food_liability_bev_model <- readRDS(prophetPath('models', 'ukTaxForesight', 'foodLiability','beverages.rds'))
food_liability_conf_model <- readRDS(prophetPath('models', 'ukTaxForesight', 'foodLiability','confectionery.rds'))


food_liability_constants <- data.frame(
  NOT_FOOD = 'Not Food',
  FOOD = 'Food',
  STANDARD = 'Standard-rated for VAT',
  ZERO = 'Zero-rated for VAT',
  YES ='Yes',
  NO = 'No',
  NONE = 'None',
  NONE_OF_THESE = 'None of these',
  HELP_ME = 'Help me predict',
  CONFECTIONERY = 'Confectionery',
  NOT_CONFECTIONERY = 'Notconfectionery',
  BEVERAGES = 'Beverages',
  NOT_BEVERAGES = 'Notbeverages',
  SNACKS = 'Snacks',
  NOT_SNACKS = 'Notsnacks',
  ICE_CREAM = 'Ice cream',
  stringsAsFactors = FALSE
)

food_liability_factors <- function(){
  
  labels <- c(attributes(food_liability_model$terms)$term.labels,
              attributes(food_liability_snack_model$terms)$term.labels,
              attributes(food_liability_bev_model$terms)$term.labels,
              attributes(food_liability_conf_model$terms)$term.labels)
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('1.0', '1.7',
                        '2.1', '2.2',
                        '3.1new', '3.2', '3.3', '3.5', '3.7', '3.8', '3.10',
                        '4.1', '4.2', '4.3', '4.4', '4.8', '4.9', '4.10', '4.11',
                        '5.1a', '5.2.1', '5.2a', '5.2b', '5.3',
                        '6.1', '6.2', '6.4')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}

food_liability_transform_data <- function(df){
  
  answered_questions <- names(df)
  df <- empty_question_transformation(df, food_liability_factors, FALSE, answered_questions)
  
}

food_liability_logic <- function(df,
                                 food_label,
                                 food_prob,
                                 snack_label,
                                 snack_prob,
                                 bev_label,
                                 bev_prob,
                                 conf_label,
                                 conf_prob){
  
  if (df$X1.0 == food_liability_constants$YES){
    food_label <- food_liability_constants$FOOD
  }
  
  
  # confectionery logic
  
  if (df$X3.1new == food_liability_constants$YES){
    confectionery_label <- food_liability_constants$CONFECTIONERY
    confectionery_prob <- 1
  } else if (df$X3.2 == food_liability_constants$NO |
             df$X3.3 == food_liability_constants$YES){
    confectionery_label <- food_liability_constants$NOT_CONFECTIONERY
    confectionery_prob <- 1
  } else {
    confectionery_label <- conf_label
    confectionery_prob <- conf_prob
  }
  
  
  # beverages logic
  
  if (df$X4.1 == food_liability_constants$YES |
      df$X4.3 == food_liability_constants$YES){
    beverages_label <- food_liability_constants$BEVERAGES
    beverages_prob <- 1
  } else if (df$X4.2 == food_liability_constants$YES |
             df$X4.4 == food_liability_constants$NO){
    beverages_label <- food_liability_constants$NOT_BEVERAGES
    beverages_prob <- 1
  } else {
    beverages_label <- bev_label
    beverages_prob <- bev_prob
  }
  
  
  # snack logic
  
  if ((df$X5.2.1 == food_liability_constants$YES &
       df$X5.3 == food_liability_constants$NO) |
      df$X5.2b == food_liability_constants$YES){
    snacks_label <- food_liability_constants$SNACKS
    snacks_prob <- 1
  } else if (df$X1.7 == food_liability_constants$SNACKS &
             df$X5.1a == food_liability_constants$NO &
             (df$X5.2.1 == food_liability_constants$NO |
              df$X5.3 == food_liability_constants$YES) &
              df$X5.2b == food_liability_constants$NO){
    snacks_label <- food_liability_constants$NOT_SNACKS
    snacks_prob <- 1
  } else {
    snacks_label <- snack_label
    snacks_prob <- snack_prob
  }
  
  
  # overall logic
  
  # Standard rated if catering or if not food
  if (df$X1.0 == food_liability_constants$NO |
      df$X6.1 == food_liability_constants$YES |
      df$X6.2 == food_liability_constants$YES |
      df$X6.4 == food_liability_constants$YES){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- 1
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             food_label == food_liability_constants$NOT_FOOD){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- food_prob
    
  # User doesn't select any exceptions
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$NONE){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- 1
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$NONE &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- food_prob
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$NONE &
             food_label == food_liability_constants$NOT_FOOD){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- food_prob
    
  # Standard rated for ice cream and chocolate covered biscuits
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             ((df$X1.7 == food_liability_constants$ICE_CREAM &
               df$X2.2 == food_liability_constants$NO) |
              (df$X1.7 == food_liability_constants$CONFECTIONERY &
               (df$X3.8 == food_liability_constants$YES &
                df$X3.5 == food_liability_constants$YES)) &
             food_label == food_liability_constants$FOOD)){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- food_prob
  } else if(df$X1.0 == food_liability_constants$YES &
            ((df$X1.7 == food_liability_constants$ICE_CREAM &
              df$X2.2 == food_liability_constants$NO) |
             (df$X1.7 == food_liability_constants$CONFECTIONERY &
              (df$X3.8 == food_liability_constants$YES &
               df$X3.5 == food_liability_constants$YES)))){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- 1
    
  # Zero rated for cakes, biscuits, items overriding the exceptions
  } else if (df$X1.0 == food_liability_constants$YES &
             ((df$X1.7 == food_liability_constants$ICE_CREAM &
               (df$X2.1 == food_liability_constants$NO |
                df$X2.2 == food_liability_constants$YES)) |
              (df$X1.7 == food_liability_constants$CONFECTIONERY &
               (df$X3.7 == food_liability_constants$YES |
                (df$X3.8 == food_liability_constants$YES &
                 df$X3.5 == food_liability_constants$NO) |
                df$X3.10 == food_liability_constants$YES)) |
              (df$X1.7 == food_liability_constants$BEVERAGES &
               (df$X4.8 == food_liability_constants$YES |
                df$X4.9 == food_liability_constants$YES |
                df$X4.10 == food_liability_constants$YES |
                df$X4.11 == food_liability_constants$YES)))){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- 1
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             ((df$X1.7 == food_liability_constants$ICE_CREAM &
               (df$X2.1 == food_liability_constants$NO |
                df$X2.2 == food_liability_constants$YES)) |
              (df$X1.7 == food_liability_constants$CONFECTIONERY &
               (df$X3.7 == food_liability_constants$YES |
                (df$X3.8 == food_liability_constants$YES &
                 df$X3.5 == food_liability_constants$NO) |
                df$X3.10 == food_liability_constants$YES)) |
              (df$X1.7 == food_liability_constants$BEVERAGES &
               (df$X4.8 == food_liability_constants$YES |
                df$X4.9 == food_liability_constants$YES |
                df$X4.10 == food_liability_constants$YES |
                df$X4.11 == food_liability_constants$YES))) &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- food_prob
    
  # Standard rated for excepted items 
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$CONFECTIONERY &
             confectionery_label == food_liability_constants$CONFECTIONERY){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- confectionery_prob
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$CONFECTIONERY &
             confectionery_label == food_liability_constants$CONFECTIONERY &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- food_prob
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$BEVERAGES &
             beverages_label == food_liability_constants$BEVERAGES){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- beverages_prob
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$BEVERAGES &
             beverages_label == food_liability_constants$BEVERAGES &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- food_prob
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$SNACKS &
             df$X5.1a == food_liability_constants$YES &
             df$X5.3 == food_liability_constants$NO &
             snacks_label == food_liability_constants$SNACKS){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- snacks_prob
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$SNACKS &
             df$X5.1a == food_liability_constants$NO &
             snacks_label == food_liability_constants$SNACKS){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- 1
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$SNACKS &
             (df$X5.1a == food_liability_constants$NO |
             (df$X5.1a == food_liability_constants$YES &
               df$X5.3 == food_liability_constants$NO)) &
             snacks_label == food_liability_constants$SNACKS &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- food_prob
    
  # Zero rated for all other food
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$CONFECTIONERY){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- confectionery_prob
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$CONFECTIONERY &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- food_prob
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$BEVERAGES){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- beverages_prob   
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$BEVERAGES &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- food_prob
  } else if (df$X1.0 == food_liability_constants$YES &
             df$X1.7 == food_liability_constants$SNACKS){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- snacks_prob   
  } else if (df$X1.0 == food_liability_constants$HELP_ME &
             df$X1.7 == food_liability_constants$SNACKS &
             food_label == food_liability_constants$FOOD){
    pred_label <- food_liability_constants$ZERO
    pred_prob <- food_prob
  } else {
    pred_label <- food_liability_constants$STANDARD
    pred_prob <- 1
  }
  
  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

food_liability_predict <- function(df){
  
  df <- food_liability_transform_data(df)
  
  food_prob_ml <- predict(food_liability_model, df, type='response')
  food_label_ml <- ifelse(food_prob_ml > 0.5, food_liability_constants$NOT_FOOD, food_liability_constants$FOOD)
  
  snack_prob_ml <- predict(food_liability_snack_model, df, type='response')
  snack_label_ml <- ifelse(snack_prob_ml > 0.5, food_liability_constants$SNACKS, food_liability_constants$NOT_SNACKS)
  
  bev_prob_ml <- predict(food_liability_bev_model, df, type='response')
  bev_label_ml <- ifelse(bev_prob_ml > 0.5, food_liability_constants$NOT_BEVERAGES, food_liability_constants$BEVERAGES)
  
  conf_prob_ml <- predict(food_liability_conf_model, df, type='response')
  conf_label_ml <- ifelse(conf_prob_ml > 0.5, food_liability_constants$NOT_CONFECTIONERY, food_liability_constants$CONFECTIONERY)
  
  hardcodes <- food_liability_logic(df, 
                                    food_label = food_label_ml, 
                                    food_prob = food_prob_ml,
                                    snack_label = snack_label_ml,
                                    snack_prob = snack_prob_ml,
                                    bev_label = bev_label_ml,
                                    bev_prob = bev_prob_ml,
                                    conf_label = conf_label_ml,
                                    conf_prob = conf_prob_ml
                                    )

  return(list(
    prediction=hardcodes$prediction,
    probability=hardcodes$probability,
    
    subResults = list(
      list(
        group = 'Food result',
        prediction = food_label_ml,
        probability=food_prob_ml
      ),
      list(
        group = 'Snacks result',
        prediction = snack_label_ml,
        probability = snack_prob_ml
      ),
      list(
        group = 'Beverages result',
        prediction = bev_label_ml,
        probability = bev_prob_ml
      ),
      list(
        group = 'Confectionery result',
        prediction = conf_label_ml,
        probability = conf_prob_ml
      )
    )
  ))
}

food_liability_interim <- function(df){
  
  df <- food_liability_transform_data(df)
  
  return(create_interim_predictor(food_liability_model,
                                  food_liability_constants$FOOD,
                                  food_liability_constants$NOT_FOOD,
                                  data.frame(),
                                  3)(df))
}
