prophetSource('interim_analysis', 'interim_analysis.R')
prophetSource('predictors', 'util', 'util.R')

uk_worker_model <- readRDS(prophetPath('models', 'ukTaxForesight', 'ukWorker','uk_worker_v15.rds'))

EMPLOYEE <- 'Employee'
SELF_EMPLOYED <- 'Self-employed'
NIC_E <- 'NIC - E'
NIC_S <- 'NIC - S'
IT_E <- 'IT - E'
IT_S <- 'IT - S'
NIC_NOT <- 'NIC status not determined by statute'
IT_NOT<- 'Income tax status not determined by statute'

YES <-'Yes'
NO <- 'No'

uk_worker_factors <- function(){
  
  labels <- attributes(uk_worker_model$terms)$term.labels
  labels <- gsub('^X', '', labels)
  hardcode_factors <- c('1.5', '1.6', '1.7_1')
  
  labels <- c(labels, hardcode_factors)
  
  return(labels)
}

uk_worker_predict <- function(df){
  
  pred_prob <- predict(uk_worker_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, SELF_EMPLOYED, EMPLOYEE)
  
  nic_label<-NIC_NOT
  nic_prob <-1
  it_label <-IT_NOT
  it_prob <- 1

  return(list(
    prediction=pred_label,
    probability=pred_prob,
    
    subResults = list(
      list(
        group = 'NIC',
        prediction = nic_label,
        probability=nic_prob
      ),
      list(
        group = 'Income Tax',
        prediction = it_label,
        probability=it_prob
      )
    )
  ))
}

uk_worker_interim <- function(df){

  return(create_interim_predictor(uk_worker_model,
                                  EMPLOYEE,
                                  SELF_EMPLOYED,
                                  data.frame(),
                                  2)(df))
}


