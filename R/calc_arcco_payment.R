crop = "peanuts"
crop_type = NULL
program_year = 2023
base_acres = NULL
mya_price = NULL
srp = NULL
erp = NULL
nmlr = NULL
oa_benchmark_yield = NULL
oa_benchmark_price = NULL
actual_revenue = NULL
historical_yields = NULL
historic_mya_prices = NULL
yield_type = NULL
cov_lvl = .85
payment_trigger_level = .86
max_payment_level = .1
state = NULL
county = NULL
fips = 1001
quiet = FALSE


#' Calculate ARC-CO Payment
#'
#' This function calculates the Agriculture Risk Coverage County Option (ARC-CO) 
#' payment for a specified crop, program year, and location. ARC-CO provides 
#' revenue-based protection at the county level.
#'
#' @param crop Character. The crop name (e.g., "corn", "soybeans", "wheat").
#' @param crop_type Character or NULL. Specific crop type if applicable (default: NULL).
#' @param program_year Numeric. The program year for which to calculate the payment.
#' @param base_acres Numeric or NULL. Number of base acres. If NULL, defaults to 1 (default: NULL).
#' @param mya_price Numeric or NULL. Marketing Year Average price. If NULL, retrieved from data (default: NULL).
#' @param srp Numeric or NULL. Statutory Reference Price. If NULL, retrieved from data (default: NULL).
#' @param erp Numeric or NULL. Effective Reference Price. If NULL, calculated or retrieved from data (default: NULL).
#' @param nmlr Numeric or NULL. National Marketing Loan Rate. If NULL, retrieved from data (default: NULL).
#' @param oa_benchmark_yield Numeric or NULL. Olympic Average benchmark yield. If NULL, calculated using get_arcco_benchmarks() (default: NULL).
#' @param oa_benchmark_price Numeric or NULL. Olympic Average benchmark price. If NULL, calculated using get_arcco_benchmarks() (default: NULL).
#' @param actual_revenue Numeric or NULL. Actual revenue for the county. If NULL, calculated using get_arcco_actual_revenue() (default: NULL).
#' @param historical_yields Numeric vector or NULL. Historical yields for yield calculation (default: NULL).
#' @param historic_mya_prices Numeric vector or NULL. Vector of 5 historic MYA prices for ERP calculation (default: NULL).
#' @param yield_type Character or NULL. Type of yield data to use (default: NULL).
#' @param cov_lvl Numeric. Coverage level as a decimal (default: 0.85 for 85%).
#' @param payment_trigger_level Numeric. Payment trigger level as a decimal (default: 0.86 for 86%).
#' @param max_payment_level Numeric. Maximum payment level as a decimal (default: 0.1 for 10%).
#' @param state Character or NULL. State name for location-specific data (default: NULL).
#' @param county Character or NULL. County name for location-specific data (default: NULL).
#' @param fips Numeric or NULL. FIPS code for location identification (default: NULL).
#' @param quiet Logical. If TRUE, suppresses warning messages (default: FALSE).
#'
#' @return Numeric. The calculated ARC-CO payment amount in dollars.
#'
#' @details
#' The ARC-CO payment is calculated using the following formula:
#' \itemize{
#'   \item Benchmark Revenue = Olympic Average Benchmark Price × Olympic Average Benchmark Yield
#'   \item If Actual Revenue ≥ (Payment Trigger Level × Benchmark Revenue): Payment Rate = 0
#'   \item If Actual Revenue ≤ (Payment Trigger Level - Max Payment Level) × Benchmark Revenue: Payment Rate = Max Payment Level × Benchmark Revenue
#'   \item Otherwise: Payment Rate = (Payment Trigger Level × Benchmark Revenue) - Actual Revenue
#'   \item ARC-CO Payment = Coverage Level × Base Acres × Payment Rate
#' }
#'
#' The function automatically retrieves missing data from internal datasets including:
#' MYA prices, statutory and effective reference prices, national marketing loan rates,
#' and calculates benchmark yields and prices using Olympic averages.
#'
#' @examples
#' \dontrun{
#' # Calculate ARC-CO payment for corn in 2023
#' payment <- calc_arcco_payment(
#'   crop = "corn",
#'   program_year = 2023,
#'   base_acres = 100,
#'   fips = 17001
#' )
#'
#' # Calculate with custom parameters
#' payment <- calc_arcco_payment(
#'   crop = "soybeans",
#'   program_year = 2023,
#'   base_acres = 250,
#'   cov_lvl = 0.80,
#'   state = "Iowa",
#'   county = "Story"
#' )
#' }
#'
#' @export
calc_arcco_payment <- function(crop,
                             crop_type = NULL,
                             program_year,
                             base_acres = NULL,
                             mya_price = NULL,
                             srp = NULL,
                             erp = NULL,
                             nmlr = NULL,
                             oa_benchmark_yield = NULL,
                             oa_benchmark_price = NULL,
                             actual_revenue = NULL,
                             historical_yields = NULL,
                             historic_mya_prices = NULL,
                             yield_type = NULL,
                             cov_lvl = .85,
                             payment_trigger_level = .86,
                             max_payment_level = .1,
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

    # First try the exact program year or 2019 for older years
    target_year <- ifelse(program_year <= 2019, 2019, program_year)
    srp = fsaEffectiveRefPrices %>%
      dplyr::filter(.data$crop == .env$crop, .data$program_year == .env$target_year)

    # If no data found for target year, fall back to most recent available year for this crop
    if(nrow(srp) == 0) {
      available_years <- fsaEffectiveRefPrices %>%
        dplyr::filter(.data$crop == .env$crop) %>%
        pull(.data$program_year)

      if(length(available_years) > 0) {
        most_recent_year <- max(available_years)
        if(!quiet) warning(paste0("No statutory reference price found for ", crop, " in ", target_year, ". Using most recent available year: ", most_recent_year))

        srp = fsaEffectiveRefPrices %>%
          dplyr::filter(.data$crop == .env$crop, .data$program_year == .env$most_recent_year)
      }
    }

    if(!is.null(crop_type)){
      srp <- srp %>%
        dplyr::filter(.data$crop_type == .env$crop_type)
    }

    srp <- srp %>% pull(statutory_reference_price)

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
        # First try the exact marketing year
        erp = fsaEffectiveRefPrices %>%
          dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year)

        # If no data found for marketing year, fall back to most recent available year for this crop
        if(nrow(erp) == 0) {
          available_years <- fsaEffectiveRefPrices %>%
            dplyr::filter(.data$crop == .env$crop) %>%
            pull(.data$program_year)

          if(length(available_years) > 0) {
            most_recent_year <- max(available_years)
            most_recent_marketing_year <- paste0(most_recent_year, "-", most_recent_year + 1)
            if(!quiet) warning(paste0("No effective reference price found for ", crop, " in ", marketing_year, ". Using most recent available year: ", most_recent_marketing_year))

            erp = fsaEffectiveRefPrices %>%
              dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$most_recent_marketing_year)
          }
        }

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

  # arco benchmark yield
  if(is.null(oa_benchmark_yield)){
    oa_benchmark_yield <- get_arcco_benchmarks(crop = crop, program_year = program_year,
                                               benchmark_type = "yield", crop_type = crop_type,
                                               yield_type = yield_type, historical_yields = historical_yields,
                                               state = state, county = county, fips = fips, quiet = quiet)
  }

  # arco benchmark price
  if(is.null(oa_benchmark_price)){
    oa_benchmark_price <- get_arcco_benchmarks(crop = crop, program_year = program_year,
                                               benchmark_type = "price", crop_type = crop_type,
                                               yield_type = yield_type, historical_prices = historic_mya_prices,
                                               erp = erp, state = state, county = county, fips = fips, quiet = quiet)
  }

  # arco actual revenue
  if(is.null(actual_revenue)){
    actual_revenue <- get_arcco_actual_revenue(crop = crop, program_year = program_year,
                                               mya_price = mya_price, nmlr = nmlr,
                                               crop_type = crop_type, yield_type = yield_type,
                                               state = state, county = county, fips = fips, quiet = quiet)
  }


  # calculate benchmark revenue
  benchmark_revenue = oa_benchmark_price * oa_benchmark_yield

  # calculate payment rate
  if(actual_revenue >= benchmark_revenue * payment_trigger_level){
    arc_payment_rate  <- 0
  } else if (actual_revenue <= (payment_trigger_level - max_payment_level) * benchmark_revenue ){
    arc_payment_rate <- max_payment_level * benchmark_revenue
  } else {
    arc_payment_rate <- (payment_trigger_level * benchmark_revenue) - actual_revenue
  }

  arc_payment <- cov_lvl * base_acres * arc_payment_rate



  # return the result
  return(arc_payment)

}





