#' Calculate ARC/PLC Payments
#'
#' This function provides an interface for calculating Agriculture Risk Coverage (ARC)
#' and Price Loss Coverage (PLC) payments under different policy environments, including
#' support for price sensitivity analysis.
#'
#' @param data Data frame, optional. Input dataset with all necessary variables.
#'   If NULL, uses the built-in fsaArcPlcData dataset.
#' @param crop Character vector. The crop name(s) for which to calculate payments
#'   (e.g., "corn", "soybeans", "wheat").
#' @param program_year Numeric vector. The program year(s) for the calculation (e.g., 2024).
#' @param policy_environment Character. Policy environment to use:
#'   \itemize{
#'     \item "fb18": Farm Bill 2018 (current policy)
#'     \item "obbb": Proposed policy with updated parameters
#'   }
#' @param payment_type Character. Type of payment calculation:
#'   \itemize{
#'     \item "plc": Price Loss Coverage only
#'     \item "arc": Agriculture Risk Coverage only
#'     \item "higher": Higher of ARC or PLC payments
#'   }
#' @param price_scenario Numeric vector, optional. Custom MYA price(s) to use instead
#'   of current prices. If NULL, uses current MYA prices from data.
#' @param price_analysis Logical. If TRUE, enables price sensitivity analysis across
#'   a range of prices (default: FALSE).
#' @param price_range Numeric vector, optional. Price range for sensitivity analysis
#'   as c(min_price, max_price, price_step). Required if price_analysis = TRUE.
#' @param sequestration_rate Numeric. Sequestration rate to apply as percentage
#'   (default: 0 for no sequestration).
#' @param fips Character vector, optional. 5-digit FIPS code(s) for county-level filtering.
#' @param state Character vector, optional. State name(s), abbreviation(s), or FIPS code(s)
#'   for state-level filtering.
#' @param county Character vector, optional. County name(s) for county-level filtering.
#'   Requires state to also be specified.
#' @param aggregate_level Character. Level of aggregation for results:
#'   \itemize{
#'     \item "total": Single total across all data
#'     \item "crop": Aggregated by crop
#'     \item "state": Aggregated by state
#'     \item "county": Aggregated by county
#'     \item "none": No aggregation, return all records
#'   }
#' @param quiet Logical. If TRUE, suppresses warning messages and progress output
#'   (default: FALSE).
#'
#' @return Data frame with payment calculations. Structure depends on aggregate_level:
#'   \itemize{
#'     \item For aggregated results: columns for grouping variables and total_payment
#'     \item For price_analysis: includes price column with payment response
#'     \item For "none": full dataset with individual payment calculations
#'   }
#'
#' @details
#' This function integrates ARC and PLC payment calculations with support for:
#'
#' **Policy Environments:**
#' \itemize{
#'   \item FB18: oa_pct=0.85, cap=1.15, payment_trigger=0.86
#'   \item OBBB: oa_pct=0.88, cap=1.15, payment_trigger=0.9, updated reference prices
#' }
#'
#' **Payment Types:**
#' \itemize{
#'   \item PLC: Based on effective reference price vs MYA price comparison
#'   \item ARC: Revenue-based protection using county benchmarks
#'   \item Higher: Maximum of ARC and PLC for optimal payment selection
#' }
#'
#' **Data Requirements:**
#' The function uses fsaArcPlcData by default, which includes comprehensive FSA data
#' with base acres, enrolled acres, yields, prices, and benchmarks. Custom data
#' should include similar variables for proper calculation.
#'
#' @examples
#' \dontrun{
#' # Basic calculation for corn in 2024 under current policy
#' payments <- calc_arc_plc_payments(
#'   crop = "corn",
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "higher"
#' )
#'
#' # Compare policies for multiple crops
#' fb18_payments <- calc_arc_plc_payments(
#'   crop = c("corn", "soybeans", "wheat"),
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "higher",
#'   aggregate_level = "crop"
#' )
#'
#' obbb_payments <- calc_arc_plc_payments(
#'   crop = c("corn", "soybeans", "wheat"),
#'   program_year = 2024,
#'   policy_environment = "obbb",
#'   payment_type = "higher",
#'   aggregate_level = "crop"
#' )
#'
#' # Price sensitivity analysis
#' price_response <- calc_arc_plc_payments(
#'   crop = "corn",
#'   program_year = 2024,
#'   policy_environment = "obbb",
#'   payment_type = "higher",
#'   price_analysis = TRUE,
#'   price_range = c(3.0, 6.0, 0.1)
#' )
#'
#' # Custom price scenario
#' custom_payments <- calc_arc_plc_payments(
#'   crop = "soybeans",
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "plc",
#'   price_scenario = 12.50
#' )
#' }
#'
#' @seealso
#' \code{\link{calc_plc_payment}} for individual PLC calculations
#' \code{\link{calc_arcco_payment}} for individual ARC-CO calculations
#' \code{\link{calc_arcco_payment_vectorized}} for vectorized ARC-CO calculations
#'
#' @export
calc_arc_plc_payments <- function(data = NULL,
                                 crop,
                                 program_year,
                                 policy_environment = c("fb18", "obbb"),
                                 payment_type = c("higher", "plc", "arc"),
                                 price_scenario = NULL,
                                 price_analysis = FALSE,
                                 price_range = NULL,
                                 sequestration_rate = 0,
                                 fips = NULL,
                                 state = NULL,
                                 county = NULL,
                                 aggregate_level = c("total", "crop", "state", "county", "none"),
                                 quiet = FALSE) {

  # Validate inputs
  policy_environment <- match.arg(policy_environment)
  payment_type <- match.arg(payment_type)
  aggregate_level <- match.arg(aggregate_level)

  # Load default data if not provided
  if (is.null(data)) {
    if (!quiet) cli::cli_inform("Using fsaArcPlcData dataset")
    data("fsaArcPlcData", envir = environment())
    data <- fsaArcPlcData
  }

  # Validate price analysis parameters
  if (price_analysis) {
    if (is.null(price_range) || length(price_range) != 3) {
      stop("price_range must be specified as c(min_price, max_price, price_step) when price_analysis = TRUE")
    }
    if (!is.null(price_scenario)) {
      cli::cli_warn("price_scenario ignored when price_analysis = TRUE")
    }
  }

  # Setup OBBB parameters if needed
  if (policy_environment == "obbb") {
    data <- setup_obbb_parameters(data)
  }

  # Filter data by location parameters
  if (!is.null(fips)) {
    data <- data %>% filter(fips %in% !!fips)
  }

  if (!is.null(state)) {
    # Handle state filtering (name, abbreviation, or FIPS)
    data <- data %>% filter(state_name %in% !!state |
                           substr(fips, 1, 2) %in% !!state)
  }

  if (!is.null(county)) {
    if (is.null(state)) {
      stop("state parameter must be specified when filtering by county")
    }
    data <- data %>% filter(county_name %in% !!county)
  }

  # Filter by crop and program year
  data <- data %>%
    filter(crop %in% !!crop, program_year %in% !!program_year)

  if (nrow(data) == 0) {
    stop("No data found matching the specified criteria")
  }

  # Price sensitivity analysis
  if (price_analysis) {
    results <- calculate_payment_response(
      data = data,
      crop = crop,
      year = program_year,
      policy_environment = policy_environment,
      payment_type = payment_type,
      price_min = price_range[1],
      price_max = price_range[2],
      price_step = price_range[3],
      sequestration_rate = sequestration_rate,
      aggregate_level = aggregate_level,
      quiet = quiet
    )
    return(results)
  }

  # Single price scenario calculation
  if (!is.null(price_scenario)) {
    results <- calc_payments_for_price(
      data_subset = data,
      test_price = price_scenario,
      policy_environment = policy_environment,
      payment_type = payment_type,
      sequestration_rate = sequestration_rate,
      quiet = quiet
    )
  } else {
    # Use current prices from data
    results <- calc_payments_for_price(
      data_subset = data,
      test_price = NULL,  # Will use current_mya_price from data
      policy_environment = policy_environment,
      payment_type = payment_type,
      sequestration_rate = sequestration_rate,
      quiet = quiet
    )
  }

  # Apply aggregation
  if (aggregate_level == "none") {
    return(results)
  }

  # Calculate total payments based on payment type and enrolled acres
  results <- results %>%
    mutate(
      total_payment_value = case_when(
        payment_type == "plc" ~ final_payment * enrolled_base_PLC,
        payment_type == "arc" ~ final_payment * enrolled_base_ARCCO,
        payment_type == "higher" ~ final_payment * (enrolled_base_PLC + enrolled_base_ARCCO),
        TRUE ~ NA_real_
      )
    )

  # Apply sequestration
  if (sequestration_rate > 0) {
    results <- results %>%
      mutate(total_payment_value = total_payment_value * (1 - sequestration_rate / 100))
  }

  # Aggregate results
  if (aggregate_level == "total") {
    summary_results <- results %>%
      summarize(
        total_payment = sum(total_payment_value, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (aggregate_level == "crop") {
    summary_results <- results %>%
      group_by(crop) %>%
      summarize(
        total_payment = sum(total_payment_value, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (aggregate_level == "state") {
    summary_results <- results %>%
      group_by(state_name) %>%
      summarize(
        total_payment = sum(total_payment_value, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (aggregate_level == "county") {
    summary_results <- results %>%
      group_by(state_name, county_name, fips) %>%
      summarize(
        total_payment = sum(total_payment_value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Add metadata
  summary_results <- summary_results %>%
    mutate(
      policy_environment = policy_environment,
      payment_type = payment_type,
      program_year = list(program_year),
      crop = list(crop)
    )

  return(summary_results)
}
