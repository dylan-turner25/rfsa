crop = "corn"
program_year = 2024
base_acres = 10
mya_price = 4.50
srp = 3.90
nmlr = 2
cov_lvl = .85
calc_plc_payment <- function(crop, program_year, base_acres,
                             mya_price = NULL, srp = NULL, erp = NULL, nmlr = NULL, plc_yield = NULL,
                             cov_lvl = .85, state = NULL, county = NULL, fips = NULL){

  # define marketing year based off of program year
  marketing_year <- paste0(program_year, "-", program_year + 1)

  # load any unsupplied data

  # mya price
  if(is.null(mya_price)){
    data("fsaMyaPrice")
    mya_price = fsaMyaPrice %>%
      dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year)  %>%
      pull(current_mya_price)
  }

  # statutory reference price
  if(is.null(srp)){
    data("fsaEffectiveRefPrices")
    srp = fsaEffectiveRefPrices %>%
      dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year) %>%
      pull(statutory_reference_price)
  }

  # effective reference price
  if(is.null(erp)){
    data("fsaEffectiveRefPrices")
    erp = fsaEffectiveRefPrices %>%
      dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year) %>%
      pull(effective_reference_price)
  }

  # non marketing loan rate
  if(is.null(nmlr)){
    data("fsaPlcPaymentRate")
    nmlr = fsaPlcPaymentRate %>%
      dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year) %>%
      pull(current_national_loan_rate)
  }

  # plc yield
  if(is.null(plc_yield)){
    data("fsaPlcPaymentRate")

    # filter on crop and marketing year
    plc_yield = fsaCountyBaseAcres %>%
      dplyr::filter(.data$crop == .env$crop, .data$program_year == .env$program_year)



  }

  #





  # if any of mya, srp, erp, nmlr are null, load fsaPLCPaymentRate.rds
  # and get the payment rate from there using crop and program year to filter,
  # multiply the payment rate by the base_acres argument



}


