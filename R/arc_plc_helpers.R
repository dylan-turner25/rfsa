#' Setup OBBB Policy Parameters
#'
#' This internal helper function adds OBBB (proposed policy) parameters to the dataset,
#' including updated statutory reference prices and national marketing loan rates.
#'
#' @param data Input data frame
#' @return Data frame with OBBB parameters added (obbb_srps, obbb_nmlr columns)
#' @keywords internal
setup_obbb_parameters <- function(data) {
  # Define OBBB statutory reference prices
  new_srps <- list(
    "wheat" = 6.35,
    "oats" = 2.65,
    "rice" = 0.1690, # medium and long grain (temperate japonica handled separately)
    "cotton" = 0.42,
    "corn" = 4.10,
    "grain sorghum" = 4.40,
    "dry peas" = 0.131,
    "peanuts" = 0.315,
    "soybeans" = 10.00,
    "barley" = 5.45,
    "chickpeas_small" = 0.2265,
    "chickpeas_large" = 0.2565,
    "lentils" = 0.2375,
    "flaxseed" = 13.10,
    "canola" = 0.2375,
    "rapeseed" = 0.2375,
    "safflower" = 0.2375,
    "mustard" = 0.2375,
    "sunflower" = 0.2375,
    "sesame" = 0.2375,
    "crambe" = 0.2375
  )

  # Define OBBB national marketing loan rates
  new_loan_rates <- list(
    "wheat" = 3.72,
    "oats" = 2.20,
    "rice" = 0.0770,
    "cotton" = 0.25,
    "corn" = 2.42,
    "grain sorghum" = 2.42,
    "dry peas" = 0.0687,
    "peanuts" = 0.195,
    "soybeans" = 6.82,
    "barley" = 2.75,
    "chickpeas_small" = 0.1100,
    "chickpeas_large" = 0.1540,
    "lentils" = 0.1430,
    "flaxseed" = 0.1110,
    "canola" = 0.1110,
    "rapeseed" = 0.1110,
    "safflower" = 0.1110,
    "mustard" = 0.1110,
    "sunflower" = 0.1110,
    "sesame" = 0.1110,
    "crambe" = 0.1110
  )

  # Create base crop names for merging (remove type-specific suffixes)
  srp_base_names <- names(new_srps)
  srp_base_names[srp_base_names == "chickpeas_small"] <- "chickpeas"
  srp_base_names[srp_base_names == "chickpeas_large"] <- "chickpeas"

  srp_obbb <- data.frame(crop = srp_base_names, obbb_srps = unlist(new_srps)) %>%
    group_by(crop) %>%
    slice(1) %>% # Take first value for duplicated crops
    ungroup()

  # Create base crop names for merging (remove type-specific suffixes)
  nmlr_base_names <- names(new_loan_rates)
  nmlr_base_names[nmlr_base_names == "chickpeas_small"] <- "chickpeas"
  nmlr_base_names[nmlr_base_names == "chickpeas_large"] <- "chickpeas"

  nmlr_obbb <- data.frame(
    crop = nmlr_base_names,
    obbb_nmlr = unlist(new_loan_rates)
  ) %>%
    group_by(crop) %>%
    slice(1) %>% # Take first value for duplicated crops
    ungroup()

  # Merge OBBB parameters with data
  data <- left_join(data, srp_obbb, by = "crop")
  data <- left_join(data, nmlr_obbb, by = "crop")

  # Add crop type specific rates using values from reference lists
  data$obbb_srps[which(
    data$crop == "chickpeas" & data$crop_type == "small"
  )] <- new_srps$chickpeas_small
  data$obbb_srps[which(
    data$crop == "chickpeas" & data$crop_type == "large"
  )] <- new_srps$chickpeas_large
  data$obbb_srps[which(
    data$crop == "rice" & data$crop_type == "temperate japonica"
  )] <- 0.1730

  data$obbb_nmlr[which(
    data$crop == "chickpeas" & data$crop_type == "small"
  )] <- new_loan_rates$chickpeas_small
  data$obbb_nmlr[which(
    data$crop == "chickpeas" & data$crop_type == "large"
  )] <- new_loan_rates$chickpeas_large
  data$obbb_nmlr[which(
    data$crop == "rice" & data$crop_type == "temperate japonica"
  )] <- data$current_national_loan_rate[which(
    data$crop == "rice" & data$crop_type == "temperate japonica"
  )]

  return(data)
}

#' Calculate Payments for Specific Price and Policy Environment
#'
#' This internal helper function calculates ARC and/or PLC payments for a specific
#' MYA price under either FB18 or OBBB policy environments.
#'
#' @param data_subset Filtered data for specific crop and year
#' @param test_price MYA price to test. If NULL, uses current_mya_price from data
#' @param policy_environment Either "fb18" or "obbb"
#' @param payment_type Either "plc", "arc", or "higher"
#' @param sequestration_rate Sequestration rate to apply as percentage
#' @param quiet Logical. If TRUE, suppresses warning messages
#' @return Data frame with payment calculations
#' @keywords internal
calc_payments_for_price <- function(data_subset, test_price = NULL, policy_environment, payment_type,
                                   sequestration_rate = 0, quiet = FALSE) {

  # Update current MYA price if test_price provided
  if (!is.null(test_price)) {
    data_subset$current_mya_price <- test_price
    data_subset$actual_revenue <- data_subset$actual_yield * test_price
  }

  # Set up policy parameters based on policy environment
  if (policy_environment == "obbb") {
    # Use OBBB parameters
    srp_col <- "obbb_srps"
    nmlr_col <- "obbb_nmlr"
    oa_pct <- 0.88
    cap <- 1.15
    max_payment_level <- 0.1
    payment_trigger_level <- 0.9
  } else {
    # Use FB18 parameters
    srp_col <- "statutory_reference_price"
    nmlr_col <- "current_national_loan_rate"
    oa_pct <- 0.85
    cap <- 1.15
    max_payment_level <- 0.1
    payment_trigger_level <- 0.86
  }

  # Prepare historical prices matrix
  historical_prices_matrix <- cbind(
    data_subset$annual_benchmark_price_lag1,
    data_subset$annual_benchmark_price_lag2,
    data_subset$annual_benchmark_price_lag3,
    data_subset$annual_benchmark_price_lag4,
    data_subset$annual_benchmark_price_lag5
  )

  # Calculate payments based on payment_type
  if (payment_type %in% c("plc", "higher")) {
    # Calculate PLC payments
    data_subset$plc_payment_calc <- unlist(lapply(1:nrow(data_subset), function(i) {
      tryCatch({
        historical_prices <- c(
          data_subset$annual_benchmark_price_lag1[i],
          data_subset$annual_benchmark_price_lag2[i],
          data_subset$annual_benchmark_price_lag3[i],
          data_subset$annual_benchmark_price_lag4[i],
          data_subset$annual_benchmark_price_lag5[i]
        )

        result <- calc_plc_payment(
          crop = data_subset$crop[i],
          crop_type = data_subset$crop_type[i],
          program_year = data_subset$program_year[i],
          base_acres = 1,
          mya_price = data_subset$current_mya_price[i],
          historic_mya_prices = historical_prices,
          srp = data_subset[[srp_col]][i],
          erp = NULL,
          always_use_erp = FALSE,
          nmlr = data_subset[[nmlr_col]][i],
          plc_yield = data_subset$plc_yield[i],
          cov_lvl = 0.85,
          fips = data_subset$fips[i],
          oa_pct = oa_pct,
          cap = cap,
          quiet = quiet
        )
        return(result)
      }, error = function(e) {
        if (!quiet) {
          cli::cli_warn("PLC calculation failed for row {i}: {e$message}")
        }
        return(NA)
      })
    }))
  }

  if (payment_type %in% c("arc", "higher")) {
    # Calculate ARC payments
    data_subset$arc_payment_calc <- calc_arcco_payment_vectorized(
      crop = data_subset$crop,
      crop_type = data_subset$crop_type,
      program_year = data_subset$program_year,
      actual_revenue = data_subset$actual_revenue,
      base_acres = 1,
      mya_price = data_subset$current_mya_price,
      srp = data_subset[[srp_col]],
      oa_benchmark_yield = data_subset$oa_bench_mark_yield,
      nmlr = data_subset[[nmlr_col]],
      historic_mya_prices = historical_prices_matrix,
      fips = data_subset$fips,
      quiet = quiet,
      max_payment_level = max_payment_level,
      oa_pct = oa_pct,
      cap = cap,
      payment_trigger_level = payment_trigger_level
    )
  }

  # Calculate final payment based on payment_type
  if (payment_type == "plc") {
    data_subset$final_payment <- data_subset$plc_payment_calc
  } else if (payment_type == "arc") {
    data_subset$final_payment <- data_subset$arc_payment_calc
  } else if (payment_type == "higher") {
    data_subset$final_payment <- pmax(data_subset$arc_payment_calc, data_subset$plc_payment_calc, na.rm = TRUE)
  }

  return(data_subset)
}

#' Calculate Payment Response to Price Changes
#'
#' This internal helper function calculates how payments respond to changes in MYA prices
#' across a specified price range, useful for price sensitivity analysis.
#'
#' @param data Input dataset with all necessary variables
#' @param crop Crop to analyze (e.g., "corn", "soybeans", "wheat")
#' @param year Program year to analyze
#' @param policy_environment Either "fb18" (current Farm Bill) or "obbb"
#' @param payment_type Either "plc", "arc", or "higher"
#' @param price_min Minimum MYA price to test
#' @param price_max Maximum MYA price to test
#' @param price_step Price increment (default 0.1)
#' @param sequestration_rate Sequestration rate to apply as percentage
#' @param aggregate_level Level of aggregation for results
#' @param quiet Logical. If TRUE, suppresses progress messages
#' @return Data frame with columns: price, total_payment, policy_environment, payment_type, crop, year
#' @keywords internal
calculate_payment_response <- function(data, crop, year, policy_environment = c("obbb", "fb18"),
                                     payment_type = c("plc", "arc", "higher"),
                                     price_min, price_max, price_step = 0.1,
                                     sequestration_rate = 0, aggregate_level = "total",
                                     quiet = FALSE) {

  # Validate inputs
  policy_environment <- match.arg(policy_environment)
  payment_type <- match.arg(payment_type)

  # Filter data by crop and year
  data_subset <- data %>%
    filter(crop %in% !!crop, program_year %in% !!year)

  if (nrow(data_subset) == 0) {
    warning(paste("No data found for crop:", paste(crop, collapse = ", "), "and year:", paste(year, collapse = ", ")))
    return(data.frame())
  }

  # Create price sequence
  price_sequence <- seq(price_min, price_max, by = price_step)

  # Initialize results
  results <- data.frame()

  # Loop over price sequence
  for (test_price in price_sequence) {
    if (!quiet) {
      cat("Processing price:", test_price, "\n")
    }

    # Calculate payments for this price
    price_results <- calc_payments_for_price(
      data_subset = data_subset,
      test_price = test_price,
      policy_environment = policy_environment,
      payment_type = payment_type,
      sequestration_rate = 0, # Apply sequestration later
      quiet = quiet
    )

    # Calculate total payment based on payment type and enrolled acres
    if (payment_type == "plc") {
      total_payment <- sum(price_results$final_payment * price_results$enrolled_base_PLC, na.rm = TRUE)
    } else if (payment_type == "arc") {
      total_payment <- sum(price_results$final_payment * price_results$enrolled_base_ARCCO, na.rm = TRUE)
    } else if (payment_type == "higher") {
      # For "higher" payment type, use sum of enrolled acres
      total_enrolled_acres <- rowSums(cbind(price_results$enrolled_base_PLC, price_results$enrolled_base_ARCCO), na.rm = TRUE)
      total_payment <- sum(price_results$final_payment * total_enrolled_acres, na.rm = TRUE)
    }

    # Apply sequestration rate
    if (sequestration_rate > 0) {
      total_payment <- total_payment * (1 - sequestration_rate / 100)
    }

    # Store results
    results <- rbind(results, data.frame(
      price = test_price,
      total_payment = total_payment,
      policy_environment = policy_environment,
      payment_type = payment_type,
      crop = paste(crop, collapse = ", "),
      year = paste(year, collapse = ", ")
    ))
  }

  return(results)
}