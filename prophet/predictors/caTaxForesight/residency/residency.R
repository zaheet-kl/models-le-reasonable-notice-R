residency_model <- readRDS(prophetPath('models', 'caTaxForesight', 'residency', 'residency3.rds'))

NOT_ORDINARILY_RESIDENT <- 'Not ordinarily resident'
ORDINARILY_RESIDENT <- 'Ordinarily resident'


residency_transform_data <- function(dfi){

  colnames(dfi) <- gsub('^X', '', colnames(dfi))

  dfi <- dfi %>%
    mutate(`2.234` = ifelse(`2.2_customChoiceValue` == 'occupied' |
                              `2.3_customChoiceValue` == 'Yes' |
                              `2.4_customChoiceValue` == 'Yes', 'Yes', 'No'))
  dfi$`2.234` <- as.factor(dfi$`2.234`)

  dfi$`2.5` <- dfi$`2.5` + 3

  dfi <- dfi %>%
    mutate(`2.55` = ifelse(`1.1` == "You left, are leaving, or plan to leave Canada during the period at issue" &
                             `2.5` < 6 &
                             `2.6` < 4,
                           `2.5` * 1.5, `2.5`))

  dfi <- dfi %>%
    mutate(`2.56` = `2.5` - `2.6`)

  dfi <- dfi %>%
    mutate(`2.77` = ifelse(`1.1` == "You left, are leaving, or plan to leave Canada during the period at issue" &
                             `2.7` == 'Yes',
                           'Yes', 'No'))
  dfi$`2.77` <- as.factor(dfi$`2.77`)

  # change default numeric values from strings to numeric
  dfi[which(dfi=="0")] <- as.numeric("0")
  dfi[which(dfi=="50")] <- as.numeric("50")

  return(dfi)
}

residency_factors <- function(){
  labels <- c("1.1",
              "1.2.0",
              "1.2",
              "1.4",
              "1.5",
              "1.5.2B",
              "1.5.2O",
              "1.5.1HK",
              "1.5SA",
              "1.6a",
              "1.6b",
              "1.6HK",
              "1.5.1.1O",
              "1.6O",
              "1.6SA",
              "2.1",
              "2.1.1_customChoiceValue",
              "2.2_customChoiceValue",
              "2.3_customChoiceValue",
              "2.4_customChoiceValue",
              "2.5",
              "2.6",
              "2.7",
              "2.8",
              "3.1",
              "3.1.1",
              "3.1.1.1",
              "3.1.2",
              "4.1_customChoiceValue",
              "4.1.1a",
              "4.1.2a",
              "4.1.1b",
              "4.1.1c",
              "4.2.2",
              "4.3",
              "5.1_customChoiceValue",
              "5.2_customChoiceValue",
              "5.3_customChoiceValue",
              "5.4_customChoiceValue",
              "5.5_customChoiceValue",
              "5.6_customChoiceValue",
              "5.7_customChoiceValue",
              "5.8_customChoiceValue",
              "5.9_customChoiceValue",
              "5.10",
              "5.11")
  return(labels)
}

residency_logic <- function(dfi, pred_label, pred_prob) {
  dfo <- data.frame(pred_label='', pred_prob='', gaugeConfidence='', confidenceClassification='', covi='', COVIConfidence='', covigaugeConfidence='', tr='',
                    TreatyresidenceConfidence='', dr='', DeemedresidenceConfidence='', ordinary='', OrdinaryresidenceConfidence='')

  i <- 1
  #COVI

  covis <- dfi[,c("5.2_customChoiceValue", "5.3_customChoiceValue", "5.4_customChoiceValue", "5.5_customChoiceValue", "5.6_customChoiceValue", "5.7_customChoiceValue", "5.8_customChoiceValue", "5.9_customChoiceValue")]

  if(dfi$`1.4` == 'Australia'){
    covis$'5.3a' = dfi$`5.3`
  }

  canadas <- sum(covis[1,] == 'Canada')
  others <- sum(covis[1,] == 'Other country of residence')
  unanswereds <- sum(covis[1,] == 'UNANSWERED')

  if(unanswereds > 7){
    dfi$covi <- 'COVI Not relevant'
    dfo$COVIConfidence <- 1
  } else if(dfi$'5.1_customChoiceValue' == 'Canada'){
    dfi$covi <- 'COVI Not relevant'
    dfo$COVIConfidence <- 1
  } else if(dfi$'5.1_customChoiceValue' == 'Other country of residence'){
    dfi$covi <- 'COVI Not relevant'
    dfo$COVIConfidence <- 1
  } else if(dfi$'1.4' != 'United States' &
            dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country'){
    dfi$covi <- 'COVI Not relevant'
    dfo$COVIConfidence <- 1
  } else if(dfi$`1.4` == 'Australia' &
            canadas == others){
    dfi$covi <- 'Canada'
    dfo$COVIConfidence <- 0.5
  } else if(canadas == others){
    dfi$covi <- 'Tied'
    dfo$COVIConfidence <- 0.5
  } else if(canadas > others){
    dfi$covi <- 'Canada'
    dfo$COVIConfidence <- canadas/(canadas + others)
  } else{
    dfi$covi <- 'Other country'
    dfo$COVIConfidence <- canadas/(canadas + others)
  }


  #Ordinary residence

  if(dfi$`2.2_customChoiceValue` == 'none' &
     dfi$`2.3_customChoiceValue` != 'Yes' &
     dfi$`2.4_customChoiceValue` != 'Yes' &
     dfi$`3.1.1` == 'Yes' &
     dfi$`3.1.2` != 0){
    dfi$ordinary <- 'Not ordinarily resident'
    pred_prob[i] <- 1
    dfo$OrdinaryresidenceConfidence <- 1
  } else{
    dfi$ordinary <- pred_label[i]
    dfo$OrdinaryresidenceConfidence <- pred_prob[i]
  }


  #Treaty residence

  if(dfi$'1.4' == 'Not listed' |
     dfi$'1.5' == 'No' |
     dfi$'1.5.2B' == 'No' |
     dfi$'1.5.2O' == 'No' |
     dfi$'1.5.1HK' == 'No' |
     dfi$'1.5SA' == 'No'){
    dfo$tr = 'TR Not relevant'
  } else if(dfi$'1.4' == 'Japan' &
            dfi$'1.5' == 'Yes'){
    dfo$tr = 'TR Mutual agreement'
  } else if((dfi$'1.4' == 'Australia' &
             dfi$'1.5' == 'Yes') &
            (dfi$'5.1_customChoiceValue' == 'Canada' |
             ((dfi$'5.1_customChoiceValue' == 'Both Canada and other country' |
               dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country') &
              dfi$covi == 'Canada'))){
    dfo$tr = 'Treaty resident'
  } else if((dfi$'1.4' == 'Australia' &
             dfi$'1.5' == 'Yes') &
            (dfi$'5.1_customChoiceValue' == 'Other country of residence' |
             ((dfi$'5.1_customChoiceValue' == 'Both Canada and other country' |
               dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country') &
              dfi$'covi' == 'Other country'))){
    dfo$tr = 'Not treaty resident'
  } else if(dfi$'1.4' == 'Bulgaria' &
            dfi$'1.5' == 'Yes' &
            dfi$covi == 'Canada'){
    dfo$tr = 'Treaty resident'
  } else if(dfi$'1.4' == 'Bulgaria' &
            dfi$'1.5' == 'Yes' &
            dfi$covi == 'Other country'){
    dfo$tr = 'TR Mutual agreement'
  } else if(dfi$'1.4' == 'Bulgaria' &
            dfi$'1.5' == 'Yes' &
            dfi$covi == 'Tied'){
    dfo$tr = 'TR Mutual agreement'
  } else if(dfi$'5.1_customChoiceValue' == 'Canada'){
    dfo$tr = 'Treaty resident'
  } else if(dfi$'5.1_customChoiceValue' == 'Other country of residence'){
    dfo$tr = 'Not treaty resident'
  } else if((dfi$'1.4' == 'United States' &
             (dfi$'5.1_customChoiceValue' == 'Both Canada and other country' |
              dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country') &
             dfi$covi == 'Canada') |
            (dfi$'1.4' != 'United States' &
             dfi$'5.1_customChoiceValue' == 'Both Canada and other country' &
             dfi$covi == 'Canada')){
    dfo$tr = 'Treaty resident'
  } else if((dfi$'1.4' == 'United States' &
             (dfi$'5.1_customChoiceValue' == 'Both Canada and other country' |
              dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country') &
             dfi$covi == 'Other country') |
            (dfi$'1.4' != 'United States' &
             dfi$'5.1_customChoiceValue' == 'Both Canada and other country' &
             dfi$covi == 'Other country')){
    dfo$tr = 'Not treaty resident'
  } else if(((dfi$'5.1_customChoiceValue' == 'Both Canada and other country' & dfi$covi == 'Tied') |
             dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country') &
            (dfi$'5.10' > dfi$'5.11')){
    dfo$tr = 'Treaty resident'
  } else if(((dfi$'5.1_customChoiceValue' == 'Both Canada and other country' &
              dfi$covi == 'Tied') |
             dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country') &
            (dfi$'5.10' < dfi$'5.11')){
    dfo$tr = 'Not treaty resident'
  } else if(((dfi$'1.4' != 'Singapore' &
              dfi$'1.4' != 'Taiwan') &
             ((dfi$'5.1_customChoiceValue' == 'Both Canada and other country' &
               dfi$covi == 'Tied') |
              dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country')) &
            (dfi$'5.10' == dfi$'5.11') &
            (dfi$'2.1' == 'Yes' &
             (dfi$'1.6a' == 'No' |
              dfi$'1.6b' == 'No' |
              dfi$'1.6HK' == 'No' |
              dfi$'1.5.1.1O' == 'No' |
              dfi$'1.6O' == 'No' |
              dfi$'1.6SA' == 'No'))){
    dfo$tr = 'Treaty resident'
  } else if(((dfi$'1.4' != 'Singapore' &
              dfi$'1.4' != 'Taiwan') &
             ((dfi$'5.1_customChoiceValue' == 'Both Canada and other country' &
               dfi$covi == 'Tied') |
              dfi$'5.1_customChoiceValue' == 'I have no permanent home in either country')) &
            (dfi$'5.10' == dfi$'5.11') &
            (dfi$'2.1' == 'No' &
             (dfi$'1.6a' == 'Yes' |
              dfi$'1.6b' == 'Yes' |
              dfi$'1.6HK' == 'Yes' |
              dfi$'1.5.1.1O' == 'Yes' |
              dfi$'1.6O' == 'Yes' |
              dfi$'1.6SA' == 'Yes'))){
    dfo$tr = 'Not treaty resident'
  } else{
    dfo$tr = 'TR Mutual agreement'
  }


  #Deemed residence

  if(dfi$'1.2.0' == 'Yes' |
     (dfi$'1.2.0' == 'Help me calculate' &
      dfi$'1.2' >= 183) |
     dfi$'4.1_customChoiceValue' == 'A member of the Canadian Forces' |
     dfi$'4.1.1a' == 'Yes' |
     dfi$'4.1.2a' == 'Yes' |
     dfi$'4.1.1b' == 'Yes' |
     dfi$'4.1.1c' == 'Yes' |
     dfi$'4.2.2' == 'No' |
     dfi$'4.3' == 'Yes'){
    dfo$dr = 'Deemed resident'
    dfo$`DeemedresidenceConfidence` <- 1
  } else{
    dfo$dr = 'Not deemed resident'
    dfo$`DeemedresidenceConfidence` <- 0
  }


  #Overall logic

  if(dfi$`4.1_customChoiceValue` == 'A member of a visiting force in Canada, within the meaning of section 22 of the Visiting Forces Act'){
    dfo$pred_label = 'Not resident in Canada'
    pred_prob[i] <- 1
  } else if((dfi$ordinary == 'Ordinarily resident' |
             dfo$dr == 'Deemed resident') &
            (dfo$tr == 'Treaty resident' |
             dfo$tr == 'TR Not relevant')){
    dfo$pred_label = 'Resident in Canada'
  } else if((dfi$ordinary == 'Ordinarily resident' |
             dfo$dr == 'Deemed resident') &
            dfo$tr == 'Not treaty resident'){
    dfo$pred_label = 'Not resident in Canada (subject to treaty analysis)'
    # pred_prob[i] <- 1.5 - pred_prob[i]
  } else if((dfi$ordinary == 'Ordinarily resident' |
             dfo$dr == 'Deemed resident') &
            dfo$tr == 'TR Mutual agreement'){
    dfo$pred_label = 'Mutual agreement required'
  } else{
    dfo$pred_label = 'Not resident in Canada'
  }


  #Confidence hardcoding

  if(dfo$dr == 'Deemed resident'){
    pred_prob[i] <- 1
  }


  if(dfo$pred_label == 'Resident in Canada'){
    dfo$confidenceClassification = dfo$pred_label
  } else{
    dfo$confidenceClassification = 'Not resident in Canada'
  }

  if(pred_prob[i] < 0.5){
    dfo$gaugeConfidence <- 1 - pred_prob[i]
  } else{
    dfo$gaugeConfidence <- pred_prob[i]
  }

  if(dfo$COVIConfidence < 0.5){
    dfo$covigaugeConfidence <- 1 - dfo$COVIConfidence
  } else{
    dfo$covigaugeConfidence <- dfo$COVIConfidence
  }

  if(dfi$covi != 'COVI Not relevant' &
     (dfo$dr == 'Deemed resident' | dfi$ordinary == 'Ordinarily resident') &
     dfo$covigaugeConfidence < dfo$gaugeConfidence){
    dfo$pred_prob <- dfo$COVIConfidence
  } else{
    dfo$pred_prob <- pred_prob[i]
  }

  if(dfo$pred_label == 'Mutual agreement required'){
    dfo$pred_prob <- 0.5
  }

  dfo$ordinary <- dfi$ordinary
  dfo$covi <- dfi$covi

  head(dfo)


  # colnames(dfo) <- c('Scored Labels', 'Scored Probabilities', 'gaugeConfidence', 'confidenceClassification', 'COVI', 'COVIConfidence', 'covigaugeConfidence',
  #                    'Treaty residence', 'Treaty residenceConfidence', 'Deemed residence', 'Deemed residenceConfidence', 'Ordinary residence', 'Ordinary residenceConfidence')

  return(dfo)
}

residency_predict <- function(dfi) {

  dfi <- residency_transform_data(dfi)

  pred_prob <- predict(residency_model, dfi, type = 'response')
  pred_label <-
    ifelse(pred_prob > 0.5,
           ORDINARILY_RESIDENT,
           NOT_ORDINARILY_RESIDENT)

  dfo <- residency_logic(dfi, pred_label, pred_prob)

  return(
    list(
      prediction = dfo$pred_label,
      probability = dfo$pred_prob,
      probabilityPrediction = dfo$confidenceClassification,
      subResults = list(
        list(
          group = 'COVI',
          prediction = dfo$covi,
          probability = dfo$COVIConfidence
        ),
        list(
          group = 'Treaty residence',
          prediction = dfo$tr,
          probability = 1
        ),
        list(
          group = 'Deemed residence',
          prediction = dfo$dr,
          probability = dfo$DeemedresidenceConfidence
        ),
        list(
          group = 'Ordinary residence',
          prediction = dfo$ordinary,
          probability = dfo$OrdinaryresidenceConfidence
        )
      )
    )
  )
}

