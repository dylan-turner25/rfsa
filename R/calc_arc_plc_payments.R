#' Calculate ARC/PLC Payments
#'
#' This function provides an interface for calculating Agriculture Risk Coverage (ARC)
#' and Price Loss Coverage (PLC) payments under different policy environments, including
#' support for price sensitivity analysis.
#'
#' @param data Data frame, optional. Input dataset with all necessary variables.
#'   If NULL, uses the built-in fsaArcPlcData dataset.
#' @param crop Character vector, optional. The crop name(s) for which to calculate payments
#'   (e.g., "corn", "soybeans", "wheat"). If NULL (default), calculates payments for all
#'   available crops and aggregates them.
#' @param program_year Numeric vector. The program year(s) for the calculation (e.g., 2024).
#' @param policy_environment Character vector. Policy environment(s) to use:
#'   \itemize{
#'     \item "fb18": Farm Bill 2018 (current policy)
#'     \item "obbb": Proposed policy with updated parameters
#'   }
#' @param payment_type Character vector. Type(s) of payment calculation:
#'   \itemize{
#'     \item "plc": Price Loss Coverage only
#'     \item "arc": Agriculture Risk Coverage only
#'     \item "higher": Higher of ARC or PLC payments
#'     \item "sum": Both ARC and PLC payments calculated separately and summed
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
#' @param aggregate_level Character vector. Level(s) of aggregation for results:
#'   \itemize{
#'     \item "total": Single total across all data
#'     \item "crop": Aggregated by crop
#'     \item "state": Aggregated by state
#'     \item "county": Aggregated by county
#'     \item "none": No aggregation, return all records
#'     \item Multiple values: e.g., c("crop", "state") aggregates by both crop and state
#'   }
#' @param quiet Logical. If TRUE, suppresses warning messages and progress output
#'   (default: FALSE).
#'
#' @return Data frame with payment calculations. Structure depends on aggregate_level:
#'   \itemize{
#'     \item For aggregated results: columns for grouping variables and total_payment
#'     \item For multiple vector parameters: separate rows for each unique combination
#'     \item For multiple aggregate levels: e.g., c("crop", "state") creates rows for each crop-state combination
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
#'   \item Sum: Both ARC and PLC calculated separately based on enrolled acres and summed
#' }
#'
#' **Data Requirements:**
#' The function uses fsaArcPlcData by default, which includes comprehensive FSA data
#' with base acres, enrolled acres, yields, prices, and benchmarks. Custom data
#' should include similar variables for proper calculation.
#'
#' @examples
#' \dontrun{
#' # Calculate payments for all crops (default behavior)
#' all_crop_payments <- calc_arc_plc_payments(
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "higher"
#' )
#'
#' # Basic calculation for corn in 2024 under current policy
#' corn_payments <- calc_arc_plc_payments(
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
#'
#' # Sum both ARC and PLC payments based on enrolled acres
#' sum_payments <- calc_arc_plc_payments(
#'   crop = "corn",
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "sum"
#' )
#'
#' # Multi-year analysis with separate rows per year
#' multi_year_payments <- calc_arc_plc_payments(
#'   crop = "corn",
#'   program_year = c(2022, 2023, 2024),
#'   policy_environment = "fb18",
#'   payment_type = "higher"
#' )
#'
#' # Multi-parameter analysis: compare policies and payment types
#' multi_param_payments <- calc_arc_plc_payments(
#'   crop = c("corn", "soybeans"),
#'   program_year = 2024,
#'   policy_environment = c("fb18", "obbb"),
#'   payment_type = c("higher", "sum"),
#'   aggregate_level = "crop"
#' )
#'
#' # Multiple price scenarios
#' price_scenarios <- calc_arc_plc_payments(
#'   crop = "wheat",
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "plc",
#'   price_scenario = c(5.00, 6.00, 7.00)
#' )
#'
#' # Multiple aggregate levels: analyze by both crop and state
#' crop_state_analysis <- calc_arc_plc_payments(
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "higher",
#'   aggregate_level = c("crop", "state")
#' )
#'
#' # Crop and county level analysis
#' detailed_analysis <- calc_arc_plc_payments(
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "higher",
#'   aggregate_level = c("crop", "county")
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
                                 crop = NULL,
                                 program_year,
                                 policy_environment = "fb18",
                                 payment_type = "higher",
                                 price_scenario = NULL,
                                 price_analysis = FALSE,
                                 price_range = NULL,
                                 sequestration_rate = 0,
                                 fips = NULL,
                                 state = NULL,
                                 county = NULL,
                                 aggregate_level = "total",
                                 quiet = FALSE) {

  # Validate inputs
  # Validate aggregate_level vector
  valid_aggregate_levels <- c("total", "crop", "state", "county", "none")
  if (!all(aggregate_level %in% valid_aggregate_levels)) {
    stop("aggregate_level must be one or more of: ", paste(valid_aggregate_levels, collapse = ", "))
  }

  # Special validation: "none" cannot be combined with other levels
  if ("none" %in% aggregate_level && length(aggregate_level) > 1) {
    stop("aggregate_level 'none' cannot be combined with other aggregate levels")
  }

  # Special validation: "total" cannot be combined with other levels (except "none" which is already handled)
  if ("total" %in% aggregate_level && length(aggregate_level) > 1) {
    stop("aggregate_level 'total' cannot be combined with other aggregate levels")
  }

  # Validate policy_environment vector
  valid_policy_envs <- c("fb18", "obbb")
  if (!all(policy_environment %in% valid_policy_envs)) {
    stop("policy_environment must be one or more of: ", paste(valid_policy_envs, collapse = ", "))
  }

  # Validate payment_type vector
  valid_payment_types <- c("higher", "plc", "arc", "sum")
  if (!all(payment_type %in% valid_payment_types)) {
    stop("payment_type must be one or more of: ", paste(valid_payment_types, collapse = ", "))
  }

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
  if (any(policy_environment == "obbb")) {
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

  # Handle crop filtering - if NULL, use all available crops
  if (is.null(crop)) {
    # Get all available crops in the data
    available_crops <- unique(data$crop)
    available_crops <- available_crops[!is.na(available_crops)]

    if (length(available_crops) == 0) {
      stop("No crops found in the data")
    }

    if (!quiet) {
      cli::cli_inform("Using all available crops: {paste(sort(available_crops), collapse = ', ')}")
    }
    crop <- available_crops
  }

  # Filter by crop and program year
  data <- data %>%
    filter(crop %in% !!crop, program_year %in% !!program_year)

  if (nrow(data) == 0) {
    stop("No data found matching the specified criteria")
  }

  # Create parameter combinations for multiple values
  if (length(policy_environment) > 1 || length(payment_type) > 1 ||
      length(program_year) > 1 || (!is.null(price_scenario) && length(price_scenario) > 1)) {

    # Create all combinations of parameters
    param_combinations <- expand.grid(
      policy_environment = policy_environment,
      payment_type = payment_type,
      program_year = program_year,
      price_scenario = if (is.null(price_scenario)) NA else price_scenario,
      stringsAsFactors = FALSE
    )

    # Process each combination and combine results
    all_results <- list()

    for (i in 1:nrow(param_combinations)) {
      combo <- param_combinations[i, ]

      # Filter data for this specific program year
      combo_data <- data %>% filter(program_year == combo$program_year)

      if (nrow(combo_data) == 0) next

      # Calculate payments for this combination
      combo_price <- if (is.na(combo$price_scenario)) NULL else combo$price_scenario

      combo_results <- calc_payments_for_price(
        data_subset = combo_data,
        test_price = combo_price,
        policy_environment = combo$policy_environment,
        payment_type = combo$payment_type,
        sequestration_rate = sequestration_rate,
        quiet = quiet
      )

      # Add parameter metadata
      combo_results$policy_environment <- combo$policy_environment
      combo_results$payment_type <- combo$payment_type
      combo_results$program_year <- combo$program_year
      if (!is.na(combo$price_scenario)) {
        combo_results$price_scenario <- combo$price_scenario
      }

      all_results[[i]] <- combo_results
    }

    # Combine all results
    results <- do.call(rbind, all_results)

    # Apply aggregation and return
    return(aggregate_multi_param_results(results, aggregate_level, sequestration_rate))
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

  # Add metadata to results for single parameter case
  results$policy_environment <- policy_environment
  results$payment_type <- payment_type
  results$program_year <- program_year

  # Apply aggregation using helper function
  return(aggregate_multi_param_results(results, aggregate_level, sequestration_rate))
}

#' Helper function to aggregate results for multiple parameter combinations
#' @param results Combined results data frame
#' @param aggregate_level Aggregation level
#' @param sequestration_rate Sequestration rate
#' @keywords internal
aggregate_multi_param_results <- function(results, aggregate_level, sequestration_rate) {

  # Calculate total payments based on payment type and enrolled acres
  results <- results %>%
    mutate(
      total_payment_value = case_when(
        payment_type == "plc" ~ final_payment * enrolled_base_PLC,
        payment_type == "arc" ~ final_payment * enrolled_base_ARCCO,
        payment_type == "higher" ~ final_payment * (enrolled_base_PLC + enrolled_base_ARCCO),
        payment_type == "sum" ~ final_payment,  # Already calculated with enrolled acres in helper
        TRUE ~ NA_real_
      )
    )

  # Apply sequestration
  if (sequestration_rate > 0) {
    results <- results %>%
      mutate(total_payment_value = total_payment_value * (1 - sequestration_rate / 100))
  }

  # Return individual records if no aggregation
  if ("none" %in% aggregate_level) {
    return(results)
  }

  # Define grouping variables based on aggregate level and available parameters
  base_groups <- c("policy_environment", "payment_type", "program_year")

  # Add price_scenario if it exists
  if ("price_scenario" %in% names(results)) {
    base_groups <- c(base_groups, "price_scenario")
  }

  # Add aggregate-specific grouping variables
  grouping_vars <- base_groups

  # Handle multiple aggregate levels (only if not "total")
  if (!"total" %in% aggregate_level) {
    if ("crop" %in% aggregate_level) {
      grouping_vars <- c(grouping_vars, "crop")
    }
    if ("state" %in% aggregate_level) {
      grouping_vars <- c(grouping_vars, "state_name")
    }
    if ("county" %in% aggregate_level) {
      grouping_vars <- c(grouping_vars, "state_name", "county_name", "fips")
    }

    # Remove duplicates in case both state and county are specified
    grouping_vars <- unique(grouping_vars)
  }

  # Perform aggregation
  summary_results <- results %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarize(
      total_payment = sum(total_payment_value, na.rm = TRUE),
      .groups = "drop"
    )

  return(summary_results)
}

