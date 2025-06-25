crop = "corn"
program_year = 2024
base_acres = 1
mya_price = NULL
srp = NULL
erp = NULL
nmlr = NULL
plc_yield = NULL
cov_lvl = .85
state = NULL
county = NULL
fips = NULL

calc_plc_payment <- function(crop,
                             program_year,
                             base_acres,
                             mya_price = NULL,
                             srp = NULL,
                             erp = NULL,
                             always_use_erp = FALSE,
                             nmlr = NULL,
                             plc_yield = NULL,
                             cov_lvl = .85,
                             state = NULL,
                             county = NULL,
                             fips = NULL,
                             level = "individual"){

  # define marketing year based off of program year
  marketing_year <- paste0(program_year, "-", program_year + 1)

  # before the unsupplied data is looked up, decide
  # if erp needs to be calculated based on if mya_price and srp are supplied
  calculate_erp = F
  if(is.null(erp) && !is.null(mya_price)){

    # check to make sure mya_price is a list with years and prices
    if(is.list(mya_price) || all(c("years", "price") %in% names(mya_price))){

      # make sure the years supplied contains at least the 5 preceeding years of program year, if there are more years, filter just to the preceeding 5
      if(length(mya_price$years) >= 5){
        # identify the 5 years preceding the program year
        target_years <- (program_year - 5):(program_year - 1)

        # find which indices in mya_price correspond to these years
        year_indices <- which(mya_price$years %in% target_years)

        # filter to just those years and corresponding prices
        mya_price$years <- mya_price$years[year_indices]
        mya_price$price <- mya_price$price[year_indices]

        # check if we have all 5 years
        if(length(mya_price$years) == 5) {
          calculate_erp <- TRUE
          message("No effective reference price supplied, calculating based on supplied MYA price and statutory reference price.")
        } else if(length(mya_price$years) < 5) {
          warning("Not enough MYA price data supplied to calculate the Effective Reference Price. Defaulting to FSA's effective reference price for the crop and marketing year.")
        }
      }
    }
  }


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
    srp = unique(fsaEffectiveRefPrices %>%
      dplyr::filter(.data$crop == .env$crop) %>%
      pull(statutory_reference_price))

    if(length(srp) > 1){
      stop(paste("Multiple statutory reference prices found for crop:", crop))
    }
  }


  # effective reference price
  if(is.null(erp)){

    if(calculate_erp){
      # Calculate ERP using helper function
      erp <- calc_effective_reference_price(mya_prices = mya_price$price, srp = srp)
    } else {
      data("fsaEffectiveRefPrices")
      erp = fsaEffectiveRefPrices %>%
        dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year) %>%
        pull(effective_reference_price)
    }



    if(length(mya_price) > 1){
      mya_price <- mya_price$price[which(mya_price$years == program_year)]
      if(length(mya_price) == 0){
        warning("No MYA price found for the specified program year. Using MYA price supplied by FSA.")
        data("fsaMyaPrice")
        mya_price = fsaMyaPrice %>%
          dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year)  %>%
          pull(current_mya_price)
      }
    }

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
    plc_yield <- get_plc_yield(crop = crop, program_year = program_year,
                               state = state, county = county, fips = fips)
  }

  # if program_year <= 2019, substitute srp for erp
  if(program_year <= 2019 & always_use_erp == F){
    erp <- srp
  }


  # calculate the payment rate
  if(erp <= mya_price){
    plc_payment_rate <- 0
  } else if (mya_price <= nmlr){
    plc_payment_rate <- (erp - nmlr)*plc_yield
  } else {
    plc_payment_rate <- (erp - mya_price)*plc_yield
  }

  # calculate final plc payment
  plc_payment <- plc_payment_rate * base_acres * cov_lvl

  # return the result
  return(plc_payment)

}

calc_plc_payment(crop = "corn",
                 program_year = 2024,
                 base_acres = 100)
