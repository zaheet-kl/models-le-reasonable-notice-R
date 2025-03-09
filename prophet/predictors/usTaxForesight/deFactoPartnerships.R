prophetSource('interim_analysis', 'interim_analysis.R')

de_facto_partnerships_model <- readRDS(prophetPath('models', 'usTaxForesight', 'deFactoPartnerships', 'dfp_lr_v8.rds'))

NOT_A_DE_FACTO_PARTNERSHIP <- 'Not a De Facto Partnership'
DE_FACTO_PARTNERSHIP <- 'De Facto Partnership'

de_facto_partnerships_predict <- function(df) {

  pred_prob <- predict(de_facto_partnerships_model, df, type='response')
  pred_label <- ifelse(pred_prob>0.5, NOT_A_DE_FACTO_PARTNERSHIP, DE_FACTO_PARTNERSHIP)

  return(list(
    prediction=pred_label,
    probability=pred_prob
  ))
}

de_facto_partnerships_interim <- create_interim_predictor(
  de_facto_partnerships_model,
  DE_FACTO_PARTNERSHIP,
  NOT_A_DE_FACTO_PARTNERSHIP,
  data.frame(),
  1
)
