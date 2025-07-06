
#' Calculate Price Loss Coverage (PLC) Payment
#'
#' This function calculates the Price Loss Coverage (PLC) payment for a specified crop,
#' program year, and base acres. The PLC program provides payments when the effective
#' reference price exceeds the higher of the loan rate or the Marketing Year Average (MYA) price.
#'
#' @param crop Character. The crop name for which to calculate the PLC payment
#'   (e.g., "corn", "soybeans", "rice", "wheat").
#' @param crop_type Character, optional. The specific crop type when applicable
#'   (e.g., "long grain" for rice, "large" for chickpeas). If not provided and
#'   multiple crop types exist, the function will average across all types with warnings.
#' @param program_year Numeric. The program year for the PLC calculation (e.g., 2024).
#' @param base_acres Numeric, optional. The number of base acres enrolled in the program.
#'   If NULL, defaults to 1 acre with a warning.
#' @param mya_price Numeric, optional. The Marketing Year Average price for the current year.
#'   If NULL, uses FSA data.
#' @param historic_mya_prices Numeric vector, optional. A vector of 5 historic MYA prices
#'   for ERP calculation. If provided, ERP will be calculated using these values instead
#'   of looking up FSA data. Must contain exactly 5 numeric values.
#' @param srp Numeric, optional. The statutory reference price. If NULL, uses FSA data.
#' @param erp Numeric, optional. The effective reference price. If NULL, uses FSA data
#'   or calculates from provided historic_mya_prices and SRP.
#' @param always_use_erp Logical. If TRUE, uses ERP even for program years <= 2019.
#'   If FALSE (default), uses SRP for program years <= 2019.
#' @param nmlr Numeric, optional. The national marketing loan rate. If NULL, uses FSA data.
#' @param plc_yield Numeric, optional. The PLC yield for the crop and location.
#'   If NULL, uses county/state/national averages based on location parameters.
#' @param cov_lvl Numeric. The coverage level as a decimal (default: 0.85 for 85%).
#' @param state Character, optional. State identifier for location-specific yields.
#'   Can be state name, abbreviation, or FIPS code.
#' @param county Character, optional. County name for county-specific yields.
#'   Requires state to also be specified.
#' @param fips Character, optional. 5-digit FIPS code for county-specific yields.
#' @param quiet Logical. If TRUE, suppresses warning messages and other
#'   non-error messages (default: FALSE). Useful for batch processing or
#'   when warnings are not needed.
#'
#' @return Numeric. The calculated PLC payment amount in dollars.
#'   Returns 0 if the effective reference price is less than or equal to the MYA price.
#'
#' @details
#' The PLC payment calculation follows this logic:
#' \\enumerate{
#'   \\item If ERP <= MYA Price: Payment = 0
#'   \\item If MYA Price <= National Marketing Loan Rate: Payment = (ERP - NMLR) × PLC Yield × Base Acres × Coverage Level
#'   \\item Otherwise: Payment = (ERP - MYA Price) × PLC Yield × Base Acres × Coverage Level
#' }
#'
#' For program years 2014-2019, the statutory reference price (SRP) is used instead of
#' the effective reference price (ERP) unless \\code{always_use_erp = TRUE}.
#'
#' When crop_type is not specified for crops with multiple types (like rice or chickpeas),
#' the function will average prices and yields across all available types and issue warnings.
#'
#' The function automatically retrieves missing data from FSA datasets:
#' \\itemize{
#'   \\item MYA prices from \\code{fsaMyaPrice}
#'   \\item Reference prices from \\code{fsaEffectiveRefPrices}
#'   \\item Marketing loan rates from \\code{fsaPlcPaymentRate}
#'   \\item PLC yields from \\code{fsaPlcYields}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic PLC payment calculation
#' calc_plc_payment(crop = "corn", program_year = 2024, base_acres = 100)
#'
#' # Rice with specific crop type
#' calc_plc_payment(crop = "rice", crop_type = "long grain",
#'                  program_year = 2024, base_acres = 150)
#'
#' # County-specific calculation
#' calc_plc_payment(crop = "soybeans", program_year = 2024, base_acres = 200,
#'                  state = "Iowa", county = "Story")
#'
#' # Using FIPS code for location
#' calc_plc_payment(crop = "wheat", program_year = 2024, base_acres = 75,
#'                  fips = "19169")
#'
#' # Custom coverage level
#' calc_plc_payment(crop = "cotton", program_year = 2024, base_acres = 50,
#'                  cov_lvl = 0.88)
#'
#' # Provide custom current MYA price and ERP
#' calc_plc_payment(crop = "corn", program_year = 2024, base_acres = 100,
#'                  mya_price = 4.50, erp = 4.30)
#'
#' # Calculate ERP from historic MYA prices
#' calc_plc_payment(crop = "corn", program_year = 2024, base_acres = 100,
#'                  mya_price = 4.50, historic_mya_prices = c(4.20, 4.30, 4.40, 4.10, 4.35))
#' }
#'
#' @seealso
#' \\code{\\link{get_plc_yield}} for PLC yield calculations
#' \\code{\\link{calc_effective_reference_price}} for ERP calculations
#'
#' @importFrom utils data
#' @export
calc_plc_payment <- function(crop,
                             crop_type = NULL,
                             program_year,
                             base_acres = NULL,
                             mya_price = NULL,
                             historic_mya_prices = NULL,
                             srp = NULL,
                             erp = NULL,
                             always_use_erp = FALSE,
                             nmlr = NULL,
                             plc_yield = NULL,
                             cov_lvl = .85,
                             state = NULL,
                             county = NULL,
                             fips = NULL,
                             quiet = FALSE){

  # if base acres is null, default to 1
  if(is.null(base_acres)){
    base_acres <- 1
    if(!quiet) warning("No base acres supplied. Defaulting to 1 base acre.")
  }

  # define marketing year based off of program year
  marketing_year <- paste0(program_year, "-", program_year + 1)

  # check if ERP should be calculated from historic MYA prices
  calculate_erp <- FALSE
  if(is.null(erp) && !is.null(historic_mya_prices)){
    # Validate historic_mya_prices input
    if(length(historic_mya_prices) != 5) {
      stop("historic_mya_prices must contain exactly 5 values")
    }
    if(!is.numeric(historic_mya_prices)) {
      stop("historic_mya_prices must be numeric")
    }
    calculate_erp <- TRUE
    if(!quiet) message("No effective reference price supplied, calculating based on provided historic MYA prices and statutory reference price.")
  }


  # load any unsupplied data

  # mya price
  if(is.null(mya_price)){
    data("fsaMyaPrice", envir = environment())

    mya_price = fsaMyaPrice %>%
      dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year)

    if(!is.null(crop_type)){
      mya_price <- mya_price %>%
        dplyr::filter(.data$crop_type == .env$crop_type)
    }

    mya_price <- mya_price %>% pull(current_mya_price)

    if(length(mya_price) > 1){
     if(!quiet) warning( paste0("No crop type supplied, taking the average MYA price across all crop types for ", crop) )
     mya_price <- mean(mya_price, na.rm = TRUE)
    }

  }

  # statutory reference price
  if(is.null(srp)){
    data("fsaEffectiveRefPrices", envir = environment())

    srp = fsaEffectiveRefPrices %>%
      dplyr::filter(.data$crop == .env$crop, .data$program_year == ifelse(.env$program_year <= 2019,2019, .env$program_year))

    if(!is.null(crop_type)){
      srp <- srp %>%
        dplyr::filter(.data$crop_type == .env$crop_type)
    }


    srp <- srp %>% pull(statutory_reference_price)

    # special logic for temperate japonica rice
    if(any(srp == .173) & program_year <= 2018){
      srp <- ifelse(srp == .173, .161, srp)
    }


    if(length(srp) > 1){
      if(!quiet) warning(paste0("No crop type supplied, taking the average statutory reference price across all crop types for ", crop))
      srp <- mean(srp, na.rm = TRUE)
    }

    if(length(srp) == 0){
      stop(paste("No statutory reference price found for crop:", crop))
    }
  }


  # effective reference price
  if(is.null(erp)){

    if(calculate_erp){
      # Calculate ERP using helper function with historic MYA prices
      erp <- calc_effective_reference_price(mya_prices = historic_mya_prices, srp = srp)
    } else {
      data("fsaEffectiveRefPrices", envir = environment())

      if(program_year >= 2019){
      erp = fsaEffectiveRefPrices %>%
        dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year)

      if(!is.null(crop_type)){
        erp <- erp %>%
          dplyr::filter(.data$crop_type == .env$crop_type)
      }

      erp <- erp %>% pull(effective_reference_price)

      if(length(erp) > 1){
        if(!quiet) warning(paste0("No crop type supplied, taking the average effective reference price across all crop types for ", crop))
        erp <- mean(erp, na.rm = TRUE)
      }

      if(length(erp) == 0){
        stop(paste("No effective reference price found for crop:", crop, "and marketing year:", marketing_year))
      }
      } else {
        erp = srp
      }


    }

  }


  # non marketing loan rate
  if(is.null(nmlr)){
    data("fsaPlcPaymentRate", envir = environment())

    nmlr = fsaPlcPaymentRate %>%
      dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year)

    if(!is.null(crop_type)){
      nmlr <- nmlr %>%
        dplyr::filter(.data$crop_type == .env$crop_type)
    }

    nmlr <- nmlr %>% pull(current_national_loan_rate)

    if(length(nmlr) > 1){
      if(!quiet) warning(paste0("No crop type supplied, taking the average national marketing loan rate across all crop types for ", crop))
      nmlr <- mean(nmlr, na.rm = TRUE)
    }

    if(length(nmlr) == 0){
      stop(paste("No national marketing loan rate found for crop:", crop, "and marketing year:", marketing_year))
    }
  }

  # plc yield
  if(is.null(plc_yield)){
    plc_yield <- get_plc_yield(crop = crop, program_year = program_year,
                               crop_type = crop_type, state = state, county = county, fips = fips, quiet = quiet)
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



