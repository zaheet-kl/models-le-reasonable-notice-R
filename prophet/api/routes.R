prophetSourceDirRecursive('predictors')

models <- list(
  deFactoPartnerships=de_facto_partnerships_model,
  reasonableNotice=reasonable_notice_model,
  reasonableNoticePerformance=reasonable_notice_performance_model,
  residency=residency_model,
  stepTransactionDoctrine=step_transaction_doctrine_model,
  assignmentOfIncomeServices=assignment_of_income_services_model,
  assignmentOfIncomeAssets=assignment_of_income_assets_model,
  continuityOfBusinessEnterprise=continuity_of_business_enterprise_model,
  tradeOrBusiness=trade_or_business_model,
  debtVsEquity=debt_vs_equity_model,
  tenThirtyOne=ten_thirty_one_model,
  usWorker=us_worker_model,
  constructiveReceipt=constructive_receipt_model,
  accuracyRelatedPenalties=accuracy_related_penalties_model,
  homeOffice=home_office_use_of_office_model,
  associatedCorporationsAndControl=associated_corporations_and_control_model,
  innocentSpouse=innocent_spouse_fairness_model,
  unrelatedBusinessIncome=unrelated_business_income_model,
  nonresidentTradeOrBusiness=nonresident_trade_or_business_model,
  insuranceArrangement=insurance_arrangement_model,
  allEventsExpense=all_events_expense_model,
  tangibleExpenditure=tangible_expenditure_repair_model,
  allEventsIncome=all_events_income_model,
  economicSubstance=economic_substance_model,
  ordinaryNecessaryBusinessExpense=ordinary_necessary_main_model,
  realEstateUS=real_estate_us_model,
  leaveOfAbsence=leave_of_absence_model,
  taxableBenefitsPrimaryBeneficiary=primary_beneficiary_model,
  relatedPersonsAndDealingAtArmsLength=related_persons_and_dealing_at_arms_length_aic_model,
  trustFundRecoveryPenalty=trust_fund_recovery_penalty_model,
  shareholderBenefits=shareholder_benefits_model,
  realEstateCA=real_estate_ca_model,
  intangibleExpenditure=intangible_expenditure_model,
  exemptFinancialService=exempt_financial_services_model,
  carryingOnBusiness=carrying_on_business_model,
  windfall=windfall_model,
  businessVsProperty=business_vs_property_model,
  directorsLiability=directors_liability_model,
  singleVsMultipleSupply=single_vs_multiple_supply_model,
  securitiesTrading=securities_trading_model,
  dutyToAccommodateDisabilities=duty_to_accommodate_disabilities_model,
  drugTesting=drug_testing_model,
  managerialExemptiontoOvertime=managerial_overtime_model,
  unitaryBusiness=unitary_business_model,
  ukWorker = uk_worker_model,
  ukdomicile=uk_domicile_model,
  foodLiability=food_liability_model,
  ukTreatyResidence=uk_treaty_residence_model
)

factors <- list(
  reasonableNotice=reasonable_notice_factors,
  reasonableNoticePerformance=reasonable_notice_performance_factors,
  residency=residency_factors,
  constructiveReceipt=constructive_receipt_factors,
  accuracyRelatedPenalties=accuracy_related_penalties_factors,
  tenThirtyOne=ten_thirty_one_factors,
  foreignAffiliateControlledForeignAffiliate=foreign_affiliate_controlled_foriegn_affiliate_factors,
  homeOffice=home_office_factors,
  associatedCorporationsAndControl=associated_corporations_and_control_factors,
  innocentSpouse=innocent_spouse_factors,
  unrelatedBusinessIncome=unrelated_business_income_factors,
  nonresidentTradeOrBusiness=nonresident_trade_or_business_factors,
  insuranceArrangement=insurance_arrangement_factors,
  compareTaxTreaties=compare_tax_treaties_factors,
  allEventsExpense=all_events_expense_factors,
  tangibleExpenditure=tangible_expenditure_factors,
  usResidency=us_residency_factors,
  allEventsIncome=all_events_income_factors,
  economicSubstance=economic_substance_factors,
  ordinaryNecessaryBusinessExpense=ordinary_necessary_factors,
  realEstateUS=real_estate_us_factors,
  qualifiedBusinessIncome199a=qbi_199a_factors,
  leaveOfAbsence=leave_of_absence_factors,
  relatedPersonsAndDealingAtArmsLength=related_persons_and_dealing_at_arms_length_factors,
  realEstateCA=real_estate_ca_factors,
  exemptFinancialServices=exempt_financial_services_factors,
  carryingOnBusiness=carrying_on_business_factors,
  windfall=windfall_factors,
  caWorker=ca_worker_factors,
  assessmentPeriod=assessment_period_factors,
  businessVsProperty=business_vs_property_factors,
  directorsLiability=directors_liability_factors,
  securitiesTrading=securities_trading_factors,
  centralManagementAndControl=central_management_and_control_factors,
  grossNegligence=gross_negligence_factors,
  dutyToAccommodateDisabilities=duty_to_accommodate_disabilities_factors,
  caEmpWorker=ca_emp_worker_factors,
  drugTesting=drug_testing_factors,
  managerialExemptiontoOvertime=managerial_overtime_factors,
  permanentEstablishment=permanent_establishment_factors_old,
  progressiveDiscipline=progressive_discipline_factors,
  saltIncomeTax=salt_income_tax_factors,
  unitaryBusiness=unitary_business_factors,
  saltNexus=salt_nexus_factors,
  tradeOrBusiness=trade_or_business_factors,
  ukWorker = uk_worker_factors,
  ukDomicile= uk_domicile_factors,
  foodLiability=food_liability_factors,
  ukTreatyResidence=uk_treaty_residence_factors
)

predictors <- list(
  deFactoPartnerships=de_facto_partnerships_predict,
  reasonableNotice=reasonable_notice_predict,
  reasonableNoticePerformance=reasonable_notice_performance_predict,
  residency=residency_predict,
  stepTransactionDoctrine=step_transaction_doctrine_predict,
  assignmentOfIncomeServices=assignment_of_income_services_predict,
  assignmentOfIncomeAssets=assignment_of_income_assets_predict,
  continuityOfBusinessEnterprise=continuity_of_business_enterprise_predict,
  tradeOrBusiness=trade_or_business_predict,
  debtVsEquity=debt_vs_equity_predict,
  tenThirtyOne=ten_thirty_one_predict,
  usWorker=us_worker_predict,
  constructiveReceipt=constructive_receipt_predict,
  accuracyRelatedPenalties=accuracy_related_penalties_predict,
  foreignAffiliateControlledForeignAffiliate=foreign_affiliate_controlled_foriegn_affiliate_predict,
  homeOffice=home_office_predict,
  associatedCorporationsAndControl=associated_corporations_and_control_predict,
  innocentSpouse=innocent_spouse_predict,
  unrelatedBusinessIncome=unrelated_business_income_predict,
  nonresidentTradeOrBusiness=nonresident_trade_or_business_predict,
  insuranceArrangement=insurance_arrangement_predict,
  compareTaxTreaties=compare_tax_treaties_predict,
  allEventsExpense=all_events_expense_predict,
  tangibleExpenditure=tangible_expenditure_predict,
  usResidency=us_residency_predict,
  allEventsIncome=all_events_income_predict,
  economicSubstance=economic_substance_predict,
  ordinaryNecessaryBusinessExpense=ordinary_necessary_predict,
  realEstateUS=real_estate_us_predict,
  qualifiedBusinessIncome199a=qbi_199a_predict,
  leaveOfAbsence=leave_of_absence_predict,
  taxableBenefitsPrimaryBeneficiary=primary_beneficiary_predict,
  relatedPersonsAndDealingAtArmsLength=related_persons_and_dealing_at_arms_length_predict,
  trustFundRecoveryPenalty=trust_fund_recovery_penalty_predict,
  shareholderBenefits=shareholder_benefits_predict,
  realEstateCA=real_estate_ca_predict,
  intangibleExpenditure=intangible_expenditure_predict,
  exemptFinancialServices=exempt_financial_services_predict,
  carryingOnBusiness=carrying_on_business_predict,
  windfall=windfall_predict,
  caWorker=ca_worker_predict,
  assessmentPeriod=assessment_period_predict,
  businessVsProperty=business_vs_property_predict,
  directorsLiability=directors_liability_predict,
  singleVsMultipleSupply=single_vs_multiple_supply_predict,
  securitiesTrading=securities_trading_predict,
  centralManagementAndControl=central_management_and_control_predict,
  grossNegligence=gross_negligence_predict,
  dutyToAccommodateDisabilities=duty_to_accommodate_disabilities_predict,
  drugTesting=drug_testing_predict,
  managerialExemptiontoOvertime=managerial_overtime_predict,
  caEmpWorker=ca_emp_worker_predict,
  permanentEstablishment=permanent_establishment_predict_old,
  progressiveDiscipline=progressive_discipline_predict,
  saltIncomeTax=salt_income_tax_predict,
  unitaryBusiness=unitary_business_predict,
  saltNexus=salt_nexus_predict,
  ukWorker = uk_worker_predict,
  ukDomicile=uk_domicile_predict,
  foodLiability=food_liability_predict,
  ukTreatyResidence=uk_treaty_residence_predict
  
)

interim_predictors <- list(
  deFactoPartnerships=de_facto_partnerships_interim,
  stepTransactionDoctrine=step_transaction_doctrine_interim,
  assignmentOfIncomeServices=assignment_of_income_services_interim,
  assignmentOfIncomeAssets=assignment_of_income_assets_interim,
  continuityOfBusinessEnterprise=continuity_of_business_enterprise_interim,
  tradeOrBusiness=trade_or_business_interim,
  debtVsEquity=debt_vs_equity_interim,
  tenThirtyOne=ten_thirty_one_interim,
  usWorker=us_worker_interim,
  constructiveReceipt=constructive_receipt_interim,
  accuracyRelatedPenalties=accuracy_related_penalties_interim,
  associatedCorporationsAndControl=associated_corporations_and_control_interim,
  homeOffice=home_office_interim_predict,
  innocentSpouse=innocent_spouse_interim,
  unrelatedBusinessIncome=unrelated_business_income_interim,
  nonresidentTradeOrBusiness=nonresident_trade_or_business_interim,
  insuranceArrangement=insurance_arrangement_interim,
  allEventsExpense=all_events_expense_interim,
  allEventsIncome=all_events_income_interim,
  economicSubstance=economic_substance_interim,
  ordinaryNecessaryBusinessExpense=ordinary_necessary_interim,
  realEstateUS=real_estate_us_interim,
  leaveOfAbsence=leave_of_absence_interim,
  taxableBenefitsPrimaryBeneficiary=primary_beneficiary_interim,
  trustFundRecoveryPenalty=trust_fund_recovery_penalty_interim,
  shareholderBenefits=shareholder_benefits_interim,
  realEstateCA=real_estate_ca_interim,
  intangibleExpenditure=intangible_expenditure_interim,
  exemptFinancialServices=exempt_financial_services_interim,
  carryingOnBusiness=carrying_on_business_interim,
  windfall=windfall_interim,
  caWorker=ca_worker_interim,
  assessmentPeriod=assessment_period_interim,
  businessVsProperty=business_vs_property_interim,
  directorsLiability=directors_liability_interim,
  singleVsMultipleSupply=single_vs_multiple_supply_interim,
  securitiesTrading=securities_trading_interim,
  centralManagementAndControl=central_management_and_control_interim,
  grossNegligence=gross_negligence_interim,
  drugTesting=drug_testing_interim,
  managerialExemptiontoOvertime=managerial_overtime_interim,
  dutyToAccommodateDisabilities=duty_to_accommodate_disabilities_interim,
  caEmpWorker=ca_emp_worker_interim,
  unitaryBusiness=unitary_business_interim,
  ukWorker = uk_worker_interim,
  ukDomicile = uk_domicile_interim,
  foodLiability=food_liability_interim,
  ukTreatyResidence=uk_treaty_residence_interim
  
)

#' @apiTitle Prediction API

#' Health Check Endpoint
#' @serializer unboxedJSON
#' @get /healthz
function() {
  list(msg="OK")
}

#' Generic Prediction Endpoint
#' @param data The object of data to pass to the classifier function
#' @serializer unboxedJSON
#' @post /<domain>/predict
function(domain, data = NULL, res) {
  predictor <- predictors[[domain]]

  log$debug('running predict', list(domain=domain, data=data))

  if (!is.list(data)) {
    send_http_error(res, 401, paste('data not serialized into a list', data))
  } else if (is.function(predictor)) {
    df <- as.data.frame.list(data)
    prediction <- predictor(df)
    log$debug('prediction result', list(domain=domain, result=prediction))
    prediction
  } else {
    send_http_error(res, 404, paste('No predictor exists for:', domain))
  }
}

#' Generic Factors Endpoint
#' @get /<domain>/factors
function(domain, res) {
  model <- models[[domain]]
  factorFn <- factors[[domain]]

  log$debug('retrieving factors', list(domain=domain))

  if (is.function(factorFn)) {
    factors <- factorFn()
    log$debug('retrieved factors', list(domain=domain, factors=factors))
    factors
  } else if (!is.null(model)) {
    labels <- attributes(model$terms)$term.labels
    factors <- gsub('^X', '', labels)
    log$debug('retrieved factors', list(domain=domain, factors=factors))
    factors
  } else {
    send_http_error(res, 404, paste('No term labels exist for:', domain, typeof(model)))
  }
}

#' Generic Interim Analysis Endpoint
#' @param data The object of data to pass to the classifier function
#' @serializer unboxedJSON
#' @post /<domain>/interim_predict
function(domain, data = NULL, res) {
  interim_predictor <- interim_predictors[[domain]]

  log$debug('running interim predict', list(domain=domain, data=data))

  if (!is.list(data)) {
    send_http_error(res, 401, paste('data not serialized into a list'))
  } else if (is.function(interim_predictor)) {
    df <- as.data.frame.list(data)
    interim_prediction <- interim_predictor(df)
    log$debug('interim prediction result', list(domain=domain, result=interim_prediction))
    interim_prediction
  } else {
    send_http_error(res, 404, paste('No interim predictor exists for:', domain))
  }
}


