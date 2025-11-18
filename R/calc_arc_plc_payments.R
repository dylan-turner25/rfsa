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
#' @param price Numeric vector, optional. Custom MYA price(s) to use instead
#'   of current prices. If NULL, uses current MYA prices from data. Can be:
#'   \itemize{
#'     \item Single value: Applied to all crops
#'     \item Vector: One price per crop (length must match number of crops)
#'   }
#' @param sequestration_rate Numeric vector. Sequestration rate(s) to apply as percentage
#'   (default: 0 for no sequestration). Can be:
#'   \itemize{
#'     \item Single value: Applied to all program years
#'     \item Vector: One sequestration rate per program year (length must match number of program years)
#'   }
#' @param fips Character vector, optional. 5-digit FIPS code(s) for county-level filtering.
#' @param state Character vector, optional. State name(s), abbreviation(s), or FIPS code(s)
#'   for state-level filtering.
#' @param county Character vector, optional. County name(s) for county-level filtering.
#'   Requires state to also be specified.
#' @param aggregate_level Character vector. Level(s) of aggregation for results:
#'   \itemize{
#'     \item "total": Single total across all data
#'     \item "crop": Aggregated by crop
#'     \item "crop_type": Aggregated by crop type (e.g., program crops vs. other oilseeds)
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
#' # Custom price scenario (single price for all crops)
#' custom_payments <- calc_arc_plc_payments(
#'   crop = "soybeans",
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "plc",
#'   price = 12.50
#' )
#'
#' # Custom price scenario (different price per crop)
#' multi_price_payments <- calc_arc_plc_payments(
#'   crop = c("corn", "soybeans", "wheat"),
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "higher",
#'   price = c(3.9, 12.5, 6.2)
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
#'
#' # Aggregate by crop type
#' crop_type_analysis <- calc_arc_plc_payments(
#'   program_year = 2024,
#'   policy_environment = "fb18",
#'   payment_type = "higher",
#'   aggregate_level = "crop_type"
#' )
#'
#' # Multi-year analysis with different sequestration rates per year
#' multi_year_seq <- calc_arc_plc_payments(
#'   crop = "corn",
#'   program_year = c(2022, 2023, 2024),
#'   policy_environment = "fb18",
#'   payment_type = "higher",
#'   sequestration_rate = c(0, 5.7, 5.7)
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
                                 price = NULL,
                                 sequestration_rate = 0,
                                 fips = NULL,
                                 state = NULL,
                                 county = NULL,
                                 aggregate_level = "total",
                                 quiet = FALSE) {

  # Validate inputs
  # Validate sequestration_rate
  if (!is.null(sequestration_rate) && length(sequestration_rate) > 1) {
    if (length(sequestration_rate) != length(program_year)) {
      stop("When sequestration_rate is a vector, its length must equal the number of program years (",
           length(program_year), "). Got ", length(sequestration_rate), " sequestration rates.")
    }
  }

  # Validate aggregate_level vector
  valid_aggregate_levels <- c("total", "crop", "crop_type", "state", "county", "none")
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

  # Setup OBBB parameters if needed
  if (any(policy_environment == "obbb")) {
    data <- setup_obbb_parameters(data)
  }

  # Filter data by location parameters
  if (!is.null(fips)) {
    data <- data %>% filter(fips %in% !!fips)
  }

  if (!is.null(state)) {
    # Handle state filtering - convert all state inputs to full state names
    # Accepts state names, abbreviations, or FIPS codes
    state_names <- character(0)

    for (s in state) {
      # Validate state input
      s <- valid_state(s)

      # Convert state input to full state name for filtering
      if (tolower(s) %in% tolower(datasets::state.abb)) {
        # Convert abbreviation to full name
        state_index <- which(tolower(datasets::state.abb) == tolower(s))
        state_names <- c(state_names, datasets::state.name[state_index])
      } else if (tolower(s) %in% tolower(datasets::state.name)) {
        # State is already a full name
        state_names <- c(state_names, s)
      } else if (nchar(s) <= 2 && suppressWarnings(!is.na(as.numeric(s)))) {
        # State is a fips code - convert to state name
        state_fips <- sprintf("%02d", as.numeric(s))
        # Get state name from fips code
        fips_to_state <- data.frame(
          fips = usmap::fips(state = datasets::state.name),
          name = datasets::state.name
        )
        state_match <- fips_to_state[fips_to_state$fips == state_fips, "name"]
        if (length(state_match) == 0) {
          stop("Invalid state fips code: ", s)
        }
        state_names <- c(state_names, state_match[1])
      } else {
        stop("Invalid state input: ", s)
      }
    }

    # Filter data using converted state names
    data <- data %>% filter(state_name %in% !!state_names)
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

  # Validate price parameter
  if (!is.null(price)) {
    # Get unique crops after filtering
    unique_crops <- unique(data$crop)
    n_crops <- length(unique_crops)

    if (length(price) > 1 && length(price) != n_crops) {
      stop("When price is a vector, its length must equal the number of crops selected (",
           n_crops, "). Got ", length(price), " prices.")
    }

    # Expand single price to all crops if needed
    if (length(price) == 1) {
      price_vector <- rep(price, n_crops)
    } else {
      price_vector <- price
    }

    # Create price mapping for each crop
    price_map <- data.frame(
      crop = unique_crops,
      custom_price = price_vector,
      stringsAsFactors = FALSE
    )

    # Join price to data
    data <- data %>%
      left_join(price_map, by = "crop")
  }

  # Create sequestration rate mapping
  if (is.null(sequestration_rate)) {
    sequestration_rate <- 0
  }

  if (length(sequestration_rate) == 1) {
    seq_rate_vector <- rep(sequestration_rate, length(program_year))
  } else {
    seq_rate_vector <- sequestration_rate
  }

  sequestration_map <- data.frame(
    program_year = program_year,
    sequestration_rate = seq_rate_vector,
    stringsAsFactors = FALSE
  )

  # Create parameter combinations for multiple values
  if (length(policy_environment) > 1 || length(payment_type) > 1 || length(program_year) > 1) {

    # Create all combinations of parameters
    param_combinations <- expand.grid(
      policy_environment = policy_environment,
      payment_type = payment_type,
      program_year = program_year,
      stringsAsFactors = FALSE
    )

    # Add sequestration rate to combinations
    param_combinations <- param_combinations %>%
      left_join(sequestration_map, by = "program_year")

    # Process each combination and combine results
    all_results <- list()

    for (i in 1:nrow(param_combinations)) {
      combo <- param_combinations[i, ]

      # Filter data for this specific program year
      combo_data <- data %>% filter(program_year == combo$program_year)

      if (nrow(combo_data) == 0) next

      # Update current MYA price if custom price provided
      if (!is.null(price)) {
        combo_data$current_mya_price <- combo_data$custom_price
        combo_data$actual_revenue <- combo_data$actual_yield * combo_data$custom_price
      }

      # Set up policy parameters
      if (combo$policy_environment == "obbb") {
        srp_col <- "obbb_srps"
        nmlr_col <- "obbb_nmlr"
        oa_pct <- 0.88
        cap <- 1.15
        max_payment_level <- 0.12
        payment_trigger_level <- 0.9
      } else {
        srp_col <- "statutory_reference_price"
        nmlr_col <- "current_national_loan_rate"
        oa_pct <- 0.85
        cap <- 1.15
        max_payment_level <- 0.1
        payment_trigger_level <- 0.86
      }

      # Prepare historical prices matrix
      historical_prices_matrix <- cbind(
        combo_data$annual_benchmark_price_lag1,
        combo_data$annual_benchmark_price_lag2,
        combo_data$annual_benchmark_price_lag3,
        combo_data$annual_benchmark_price_lag4,
        combo_data$annual_benchmark_price_lag5
      )

      # Calculate payments based on payment_type
      # Initialize columns to ensure consistent structure
      if (combo$payment_type %in% c("plc", "higher", "sum")) {
        combo_data$plc_payment_calc <- calc_plc_payment_vectorized(
          crop = combo_data$crop,
          crop_type = combo_data$crop_type,
          program_year = combo_data$program_year,
          base_acres = 1,
          mya_price = combo_data$current_mya_price,
          historic_mya_prices = historical_prices_matrix,
          srp = combo_data[[srp_col]],
          erp = NULL,
          nmlr = combo_data[[nmlr_col]],
          plc_yield = combo_data$plc_yield,
          cov_lvl = 0.85,
          fips = combo_data$fips,
          oa_pct = oa_pct,
          cap = cap,
          quiet = quiet
        )
      } else {
        # Initialize plc_payment_calc as NA for non-PLC calculations
        combo_data$plc_payment_calc <- NA_real_
      }

      if (combo$payment_type %in% c("arc", "higher", "sum")) {
        combo_data$arc_payment_calc <- calc_arcco_payment_vectorized(
          crop = combo_data$crop,
          crop_type = combo_data$crop_type,
          program_year = combo_data$program_year,
          actual_revenue = combo_data$actual_revenue,
          base_acres = 1,
          mya_price = combo_data$current_mya_price,
          srp = combo_data[[srp_col]],
          oa_benchmark_yield = combo_data$oa_bench_mark_yield,
          nmlr = combo_data[[nmlr_col]],
          historic_mya_prices = historical_prices_matrix,
          fips = combo_data$fips,
          quiet = quiet,
          max_payment_level = max_payment_level,
          oa_pct = oa_pct,
          cap = cap,
          payment_trigger_level = payment_trigger_level
        )
      } else {
        # Initialize arc_payment_calc as NA for non-ARC calculations
        combo_data$arc_payment_calc <- NA_real_
      }

      # Calculate final payment based on payment_type
      if (combo$payment_type == "plc") {
        combo_data$final_payment <- combo_data$plc_payment_calc
      } else if (combo$payment_type == "arc") {
        combo_data$final_payment <- combo_data$arc_payment_calc
      } else if (combo$payment_type == "higher") {
        combo_data$final_payment <- pmax(combo_data$arc_payment_calc, combo_data$plc_payment_calc, na.rm = TRUE)
      } else if (combo$payment_type == "sum") {
        combo_data$final_payment <- (combo_data$plc_payment_calc * combo_data$enrolled_base_PLC) +
                                     (combo_data$arc_payment_calc * combo_data$enrolled_base_ARCCO)
      }

      # Add parameter metadata
      combo_data$policy_environment <- combo$policy_environment
      combo_data$payment_type <- combo$payment_type
      combo_data$program_year <- combo$program_year
      combo_data$sequestration_rate <- combo$sequestration_rate

      all_results[[i]] <- combo_data
    }

    # Combine all results
    results <- do.call(rbind, all_results)

    # Apply aggregation and return
    return(aggregate_multi_param_results(results, aggregate_level))
  }

  # Single parameter calculation - inlined logic
  # Update current MYA price if custom price provided
  if (!is.null(price)) {
    data$current_mya_price <- data$custom_price
    data$actual_revenue <- data$actual_yield * data$custom_price
  }

  # Set up policy parameters
  if (policy_environment == "obbb") {
    srp_col <- "obbb_srps"
    nmlr_col <- "obbb_nmlr"
    oa_pct <- 0.88
    cap <- 1.15
    max_payment_level <- 0.1
    payment_trigger_level <- 0.9
  } else {
    srp_col <- "statutory_reference_price"
    nmlr_col <- "current_national_loan_rate"
    oa_pct <- 0.85
    cap <- 1.15
    max_payment_level <- 0.1
    payment_trigger_level <- 0.86
  }

  # Prepare historical prices matrix
  historical_prices_matrix <- cbind(
    data$annual_benchmark_price_lag1,
    data$annual_benchmark_price_lag2,
    data$annual_benchmark_price_lag3,
    data$annual_benchmark_price_lag4,
    data$annual_benchmark_price_lag5
  )

  # Calculate payments based on payment_type
  if (payment_type %in% c("plc", "higher", "sum")) {
    data$plc_payment_calc <- calc_plc_payment_vectorized(
      crop = data$crop,
      crop_type = data$crop_type,
      program_year = data$program_year,
      base_acres = 1,
      mya_price = data$current_mya_price,
      historic_mya_prices = historical_prices_matrix,
      srp = data[[srp_col]],
      erp = NULL,
      nmlr = data[[nmlr_col]],
      plc_yield = data$plc_yield,
      cov_lvl = 0.85,
      fips = data$fips,
      oa_pct = oa_pct,
      cap = cap,
      quiet = quiet
    )
  }

  if (payment_type %in% c("arc", "higher", "sum")) {
    data$arc_payment_calc <- calc_arcco_payment_vectorized(
      crop = data$crop,
      crop_type = data$crop_type,
      program_year = data$program_year,
      actual_revenue = data$actual_revenue,
      base_acres = 1,
      mya_price = data$current_mya_price,
      srp = data[[srp_col]],
      oa_benchmark_yield = data$oa_bench_mark_yield,
      nmlr = data[[nmlr_col]],
      historic_mya_prices = historical_prices_matrix,
      fips = data$fips,
      quiet = quiet,
      max_payment_level = max_payment_level,
      oa_pct = oa_pct,
      cap = cap,
      payment_trigger_level = payment_trigger_level
    )
  }

  # Calculate final payment based on payment_type
  if (payment_type == "plc") {
    data$final_payment <- data$plc_payment_calc
  } else if (payment_type == "arc") {
    data$final_payment <- data$arc_payment_calc
  } else if (payment_type == "higher") {
    data$final_payment <- pmax(data$arc_payment_calc, data$plc_payment_calc, na.rm = TRUE)
  } else if (payment_type == "sum") {
    data$final_payment <- (data$plc_payment_calc * data$enrolled_base_PLC) +
                          (data$arc_payment_calc * data$enrolled_base_ARCCO)
  }

  results <- data

  # Add metadata to results for single parameter case
  results$policy_environment <- policy_environment
  results$payment_type <- payment_type
  results$program_year <- program_year

  # Add sequestration rate from the mapping
  results <- results %>%
    left_join(sequestration_map, by = "program_year")

  # Apply aggregation using helper function
  return(aggregate_multi_param_results(results, aggregate_level))
}

#' Helper function to aggregate results for multiple parameter combinations
#' @param results Combined results data frame
#' @param aggregate_level Aggregation level
#' @keywords internal
aggregate_multi_param_results <- function(results, aggregate_level) {

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

  # Apply sequestration (use sequestration_rate from results data frame)
  results <- results %>%
    mutate(total_payment_value = total_payment_value * (1 - sequestration_rate / 100))

  # Return individual records if no aggregation
  if ("none" %in% aggregate_level) {
    return(results)
  }

  # Define grouping variables based on aggregate level and available parameters
  base_groups <- c("policy_environment", "payment_type", "program_year")

  # Add aggregate-specific grouping variables
  grouping_vars <- base_groups

  # Handle multiple aggregate levels (only if not "total")
  if (!"total" %in% aggregate_level) {
    if ("crop" %in% aggregate_level) {
      grouping_vars <- c(grouping_vars, "crop")
    }
    if ("crop_type" %in% aggregate_level) {
      grouping_vars <- c(grouping_vars, "crop_type")
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

