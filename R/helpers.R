
#' Get PLC Yield Data
#'
#' This internal helper function retrieves and filters PLC yield data from the
#' fsaPlcYields dataset based on crop, program year, and optional location parameters.
#' It handles flexible location input including state names, abbreviations, FIPS codes,
#' and county specifications.
#'
#' @param crop Character. The crop name to filter for (e.g., "corn", "soybeans").
#' @param program_year Numeric. The program year to filter for (e.g., 2024).
#' @param crop_type Character, optional. The crop type to filter for (e.g., "long grain", "large").
#' @param state Character, optional. State identifier that can be:
#'   \\itemize{
#'     \\item State abbreviation (e.g., "IA", "IL")
#'     \\item Full state name (e.g., "Iowa", "Illinois")
#'     \\item State FIPS code (e.g., "19", "17")
#'   }
#' @param county Character, optional. County name (requires state to also be specified).
#' @param fips Character, optional. 5-digit FIPS code for specific county-level data.
#' @param quiet Logical. If TRUE, suppresses warning messages (default: FALSE).
#'
#' @return Numeric. The PLC yield value. Returns:
#'   \\itemize{
#'     \\item County-specific yield if fips or state+county provided
#'     \\item State average yield if only state provided (with message)
#'     \\item National average yield if no location specified (with message)
#'   }
#'
#' @details
#' The function follows this priority order for location filtering:
#' \\enumerate{
#'   \\item If \\code{fips} is provided, uses it directly after validation
#'   \\item If \\code{state} and \\code{county} are provided, filters by both
#'   \\item If only \\code{state} is provided, calculates state average
#'   \\item If no location parameters, calculates national average
#' }
#'
#' State abbreviations are automatically converted to full state names since the
#' underlying data uses full state names for filtering.
#'
#' @examples
#' \dontrun{
#' # County-specific yield using FIPS
#' get_plc_yield("corn", 2024, fips = "19001")
#'
#' # County-specific yield using state + county with crop type
#' get_plc_yield("rice", 2024, crop_type = "long grain", state = "IA", county = "Adair")
#'
#' # State average (abbreviation converted to full name)
#' get_plc_yield("corn", 2024, state = "IA")
#'
#' # National average with crop type
#' get_plc_yield("rice", 2024, crop_type = "long grain")
#' }
#'
#' @importFrom utils data
#' @keywords internal
get_plc_yield <- function(crop, program_year, crop_type = NULL, state = NULL, county = NULL, fips = NULL, quiet = FALSE) {
  data("fsaPlcYields", envir = environment())

  # filter on crop and marketing year
  # note: plc average county yields aren't avaliable prior to 2018, so function defaults to 2018 for earlier years
  plc_yield = fsaPlcYields %>%
    dplyr::filter(.data$crop == .env$crop,
                  .data$program_year == ifelse(.env$program_year <= 2018, 2018, .env$program_year) )

  # filter by crop_type if provided
  if(!is.null(crop_type) && length(crop_type) > 0 && !is.na(crop_type)){
    plc_yield <- plc_yield %>%
      dplyr::filter(.data$crop_type == .env$crop_type)
  }

  # filter the yield data based on location input
  if (!is.null(fips)) {
    # Use fips if provided - clean and validate using clean_fips approach
    fips_clean <- clean_fips(fips = fips)
    plc_yield <- plc_yield %>% dplyr::filter(.data$fips == fips_clean)
    if(!quiet) warning("No PLC yield supplied. Using county average PLC yield for calculations.")
  } else if (!is.null(state)) {
    # Validate state input (accepts names, abbreviations, or fips codes)
    state <- valid_state(state)

    # Convert state abbreviation to full name if needed (since data has full names)
    if (tolower(state) %in% tolower(datasets::state.abb)) {
      # Convert abbreviation to full name
      state_index <- which(tolower(datasets::state.abb) == tolower(state))
      state_name <- datasets::state.name[state_index]
    } else if (tolower(state) %in% tolower(datasets::state.name)) {
      # State is already a full name
      state_name <- state
    } else if (nchar(state) <= 2 && suppressWarnings(!is.na(as.numeric(state)))) {
      # State is a fips code - need to convert to state name
      state_fips <- sprintf("%02d", as.numeric(state))
      # Get state name from fips code
      fips_to_state <- data.frame(
        fips = usmap::fips(state = datasets::state.name),
        name = datasets::state.name
      )
      state_match <- fips_to_state[fips_to_state$fips == state_fips, "name"]
      if (length(state_match) == 0) {
        stop("Invalid state fips code: ", state)
      }
      state_name <- state_match[1]
    } else {
      stop("Invalid state input: ", state)
    }

    if (!is.null(county)) {
      # Both state and county provided - filter by state name and county
      plc_yield <- plc_yield %>% dplyr::filter(.data$state == state_name, .data$county == county)
    } else {
      # Only state provided - filter by state name and average
      plc_yield <- plc_yield %>% dplyr::filter(.data$state == state_name) %>%
        summarize(plc_yield = mean(.data$plc_yield, na.rm = TRUE))
      if(!quiet) warning("No PLC yield or county supplied. Using state average PLC yield for calculations.")
    }
  } else {
    # No location filtering - use national average
    if(!quiet) warning("No PLC yield or location parameters supplied. Using national average PLC yield for calculations.")
    plc_yield <- plc_yield %>%
      summarize(plc_yield = mean(.data$plc_yield, na.rm = TRUE))  # Average yield for national
  }

  # Check if we have multiple yields and need to warn about averaging across crop types
  if(is.null(crop_type) && nrow(plc_yield) > 1 && "plc_yield" %in% names(plc_yield)){
    if(!quiet) warning(paste0("No crop type supplied, taking the average PLC yield across all crop types for ", crop))
    plc_yield <- plc_yield %>%
      summarize(plc_yield = mean(.data$plc_yield, na.rm = TRUE))
  }

  # Ensure we return a single value
  if(nrow(plc_yield) > 1) {
    plc_yield <- plc_yield %>%
      summarize(plc_yield = mean(.data$plc_yield, na.rm = TRUE))
  }

  return(plc_yield$plc_yield[1])
}

#' Get ARC-CO Benchmark Data
#'
#' This internal helper function retrieves and filters ARC-CO benchmark data (yield or price) from the
#' fsaArcCoBenchmarks dataset based on crop, program year, yield type, and optional location parameters.
#' It handles flexible location input including state names, abbreviations, FIPS codes,
#' and county specifications.
#'
#' @param crop Character. The crop name to filter for (e.g., "corn", "soybeans").
#' @param program_year Numeric. The program year to filter for (e.g., 2024).
#' @param benchmark_type Character. Type of benchmark to return: "yield" or "price" (default: "yield").
#' @param crop_type Character, optional. The crop type to filter for (e.g., "long grain", "large").
#' @param yield_type Character, optional. The yield type to filter for from the yield_type column.
#' @param historical_yields Numeric vector, optional. A vector of 5 historic yield values
#'   for Olympic average calculation. If provided, the function will calculate the Olympic
#'   average (removing highest and lowest values) instead of looking up FSA data.
#'   Only used when benchmark_type = "yield".
#' @param historical_prices Numeric vector, optional. A vector of 5 historic price values
#'   for Olympic average calculation. If provided, the function will calculate the Olympic
#'   average (removing highest and lowest values) instead of looking up FSA data.
#'   Only used when benchmark_type = "price".
#' @param erp Numeric, optional. The effective reference price. When benchmark_type = "price",
#'   any annual benchmark price below this ERP value will be replaced with the ERP before
#'   calculating the Olympic average. If NULL, the function will look up ERP from FSA data.
#' @param state Character, optional. State identifier that can be:
#'   \\itemize{
#'     \\item State abbreviation (e.g., "IA", "IL")
#'     \\item Full state name (e.g., "Iowa", "Illinois")
#'     \\item State FIPS code (e.g., "19", "17")
#'   }
#' @param county Character, optional. County name (requires state to also be specified).
#' @param fips Character, optional. 5-digit FIPS code for specific county-level data.
#' @param quiet Logical. If TRUE, suppresses warning messages (default: FALSE).
#'
#' @return Numeric. The ARC-CO benchmark value (yield or price). Returns:
#'   \\itemize{
#'     \\item County-specific value if fips or state+county provided
#'     \\item State average value if only state provided (with message)
#'     \\item National average value if no location specified (with message)
#'   }
#'
#' @details
#' The function has two main modes of operation:
#'
#' **Historical Data Mode**: When historical_yields (for yield) or historical_prices (for price)
#' are provided, the function calculates an Olympic average by removing the highest and lowest
#' values and averaging the remaining three values. For price calculations, if an ERP is provided,
#' any historical price below the ERP will be replaced with the ERP before calculating the Olympic average.
#'
#' **FSA Data Lookup Mode**: When no historical data is provided, the function follows this
#' priority order for location filtering:
#' \\enumerate{
#'   \\item If \\code{fips} is provided, uses it directly after validation
#'   \\item If \\code{state} and \\code{county} are provided, filters by both
#'   \\item If only \\code{state} is provided, calculates state average
#'   \\item If no location parameters, calculates national average
#' }
#'
#' For benchmark price calculations, if the FSA benchmark price is below the ERP, it will be
#' replaced with the ERP value. If no ERP is provided, the function will attempt to look it up
#' from FSA effective reference price data.
#'
#' If no yield_type is specified and multiple yield types are found, the function
#' will average across all yield types and issue a warning.
#'
#' State abbreviations are automatically converted to full state names since the
#' underlying data uses full state names for filtering.
#'
#' @examples
#' \dontrun{
#' # County-specific yield using FIPS
#' get_arcco_benchmarks("corn", 2024, "yield", fips = "19001")
#'
#' # County-specific price using state + county with crop type and yield type
#' get_arcco_benchmarks("rice", 2024, "price", crop_type = "long grain", yield_type = "irrigated",
#'                      state = "IA", county = "Adair")
#'
#' # State average yield (abbreviation converted to full name)
#' get_arcco_benchmarks("corn", 2024, "yield", state = "IA")
#'
#' # National average price with crop type
#' get_arcco_benchmarks("rice", 2024, "price", crop_type = "long grain")
#'
#' # Olympic average calculation using historical yields
#' get_arcco_benchmarks("corn", 2024, "yield", historical_yields = c(170, 175, 180, 185, 190))
#'
#' # Olympic average calculation using historical prices
#' get_arcco_benchmarks("corn", 2024, "price", historical_prices = c(4.20, 4.30, 4.40, 4.50, 4.60))
#' }
#'
#' @importFrom utils data
#' @keywords internal
get_arcco_benchmarks <- function(crop, program_year, benchmark_type = "yield", crop_type = NULL, yield_type = NULL, historical_yields = NULL, historical_prices = NULL, erp = NULL, state = NULL, county = NULL, fips = NULL, quiet = FALSE) {
  # Validate benchmark_type
  if(!benchmark_type %in% c("yield", "price")) {
    stop("benchmark_type must be either 'yield' or 'price'")
  }

  # If historical data is provided, calculate Olympic average instead of looking up data
  if(benchmark_type == "yield" && !is.null(historical_yields)) {
    # Validate historical yields input
    if(length(historical_yields) != 5) {
      stop("historical_yields must contain exactly 5 values")
    }
    if(!is.numeric(historical_yields)) {
      stop("historical_yields must be numeric")
    }
    # Calculate Olympic average (remove highest and lowest, average the rest)
    sorted_yields <- sort(historical_yields)
    if(length(sorted_yields) >= 3) {
      olympic_average <- mean(sorted_yields[2:(length(sorted_yields)-1)])
    } else if(length(sorted_yields) > 0) {
      # If fewer than 3 values, just use the mean of all available values
      olympic_average <- mean(sorted_yields)
    } else {
      stop("No valid historical yields provided for benchmark calculation")
    }
    if(!quiet) message("Using Olympic average of provided historical yields for benchmark yield calculation.")
    return(olympic_average)
  }

  if(benchmark_type == "price" && !is.null(historical_prices)) {
    # Validate historical prices input
    if(length(historical_prices) != 5) {
      stop("historical_prices must contain exactly 5 values")
    }
    if(!is.numeric(historical_prices)) {
      stop("historical_prices must be numeric")
    }

    # If ERP is provided, apply the adjustment: replace any price < ERP with ERP
    adjusted_prices <- historical_prices
    if(!is.null(erp)) {
      if(!is.numeric(erp) || length(erp) != 1) {
        stop("erp must be a single numeric value")
      }
      # Replace any benchmark price below ERP with ERP value
      prices_below_erp <- !is.na(adjusted_prices) & adjusted_prices < erp
      if(any(prices_below_erp, na.rm = TRUE)) {
        adjusted_prices[prices_below_erp] <- erp
        if(!quiet) message("Replaced ", sum(prices_below_erp, na.rm = TRUE), " benchmark price(s) below ERP (", erp, ") with ERP value.")
      }
    } else {
      if(!quiet) message("No ERP provided - using raw historical prices for Olympic average calculation.")
    }

    # Calculate Olympic average (remove highest and lowest, average the rest)
    sorted_prices <- sort(adjusted_prices)
    if(length(sorted_prices) >= 3) {
      olympic_average <- mean(sorted_prices[2:(length(sorted_prices)-1)])
    } else if(length(sorted_prices) > 0) {
      # If fewer than 3 values, just use the mean of all available values
      olympic_average <- mean(sorted_prices)
    } else {
      stop("No valid historical prices provided for benchmark calculation")
    }
    if(!quiet) message("Using Olympic average of provided historical prices for benchmark price calculation.")
    return(olympic_average)
  }

  data("fsaArcCoBenchmarks", envir = environment())

  # filter on crop and marketing year
  # note: arc-co average county benchmarks aren't available prior to 2014, so function defaults to 2014 for earlier years
  arcco_data = fsaArcCoBenchmarks %>%
    dplyr::filter(.data$crop == .env$crop,
                  .data$program_year == ifelse(.env$program_year <= 2014, 2014, .env$program_year) )

  # filter by crop_type if provided
  if(!is.null(crop_type) && length(crop_type) > 0 && !is.na(crop_type)){
    arcco_data <- arcco_data %>%
      dplyr::filter(.data$crop_type == .env$crop_type)
  }

  # filter by yield_type if provided
  if(!is.null(yield_type)){
    arcco_data <- arcco_data %>%
      dplyr::filter(.data$yield_type == .env$yield_type)
  }

  # Determine column name based on benchmark_type
  column_name <- ifelse(benchmark_type == "yield", "oa_bench_mark_yield", "oa_bench_mark_price")

  # filter the data based on location input
  if (!is.null(fips)) {
    # Use fips if provided - clean and validate using clean_fips approach
    fips_clean <- clean_fips(fips = fips)
    arcco_data <- arcco_data %>% dplyr::filter(.data$fips == fips_clean)
    if(!quiet) warning(paste("No ARC-CO", benchmark_type, "supplied. Using county average ARC-CO benchmark", benchmark_type, "for calculations."))
  } else if (!is.null(state)) {
    # Validate state input (accepts names, abbreviations, or fips codes)
    state <- valid_state(state)

    # Convert state abbreviation to full name if needed (since data has full names)
    if (tolower(state) %in% tolower(datasets::state.abb)) {
      # Convert abbreviation to full name
      state_index <- which(tolower(datasets::state.abb) == tolower(state))
      state_name <- datasets::state.name[state_index]
    } else if (tolower(state) %in% tolower(datasets::state.name)) {
      # State is already a full name
      state_name <- state
    } else if (nchar(state) <= 2 && suppressWarnings(!is.na(as.numeric(state)))) {
      # State is a fips code - need to convert to state name
      state_fips <- sprintf("%02d", as.numeric(state))
      # Get state name from fips code
      fips_to_state <- data.frame(
        fips = usmap::fips(state = datasets::state.name),
        name = datasets::state.name
      )
      state_match <- fips_to_state[fips_to_state$fips == state_fips, "name"]
      if (length(state_match) == 0) {
        stop("Invalid state fips code: ", state)
      }
      state_name <- state_match[1]
    } else {
      stop("Invalid state input: ", state)
    }

    if (!is.null(county)) {
      # Both state and county provided - filter by state name and county
      arcco_data <- arcco_data %>% dplyr::filter(.data$state == state_name, .data$county == county)
    } else {
      # Only state provided - filter by state name and average
      arcco_data <- arcco_data %>% dplyr::filter(.data$state == state_name) %>%
        summarize(!!column_name := mean(.data[[column_name]], na.rm = TRUE))
      if(!quiet) warning(paste("No county supplied. Using state average ARC-CO benchmark", benchmark_type, "for calculations."))
    }
  } else {
    # No location filtering - use national average
    if(!quiet) warning(paste("No location parameters supplied. Using national average ARC-CO benchmark", benchmark_type, "for calculations."))
    arcco_data <- arcco_data %>%
      summarize(!!column_name := mean(.data[[column_name]], na.rm = TRUE))  # Average for national
  }

  # Check if we have multiple values and need to warn about averaging across types
  if(nrow(arcco_data) > 1 && column_name %in% names(arcco_data)){
    if(is.null(crop_type) && is.null(yield_type)){
      if(!quiet) warning(paste0("No crop type or yield type supplied, taking the average ARC-CO benchmark ", benchmark_type, " across all types for ", crop))
    } else if(is.null(crop_type)){
      if(!quiet) warning(paste0("No crop type supplied, taking the average ARC-CO benchmark ", benchmark_type, " across all crop types for ", crop))
    } else if(is.null(yield_type)){
      if(!quiet) warning(paste0("No yield type supplied, taking the average ARC-CO benchmark ", benchmark_type, " across all yield types for ", crop))
    } else {
      if(!quiet) warning(paste0("Multiple ", benchmark_type, " values identified, taking the average ARC-CO benchmark ", benchmark_type, " for ", crop))
    }
    # Take the average when multiple rows exist
    arcco_data <- arcco_data %>%
      summarize(!!column_name := mean(.data[[column_name]], na.rm = TRUE))
  }

  # For benchmark price calculations using FSA data, apply ERP adjustment if needed
  if(benchmark_type == "price" && is.null(historical_prices)) {
    benchmark_price <- arcco_data[[column_name]]

    # Skip ERP adjustment if benchmark price is missing
    if(is.null(benchmark_price) || length(benchmark_price) == 0 || is.na(benchmark_price)) {
      return(arcco_data[[column_name]])
    }

    # Look up ERP if not provided
    if(is.null(erp)) {
      # Define marketing year based off of program year
      marketing_year <- paste0(program_year, "-", program_year + 1)

      # Look up ERP using same logic as calc_arcco_payment
      tryCatch({
        data("fsaEffectiveRefPrices", envir = environment())

        if(program_year >= 2019){
          # First try the exact marketing year
          erp_data = fsaEffectiveRefPrices %>%
            dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$marketing_year)

          # If no data found for marketing year, fall back to most recent available year for this crop
          if(nrow(erp_data) == 0) {
            available_years <- fsaEffectiveRefPrices %>%
              dplyr::filter(.data$crop == .env$crop) %>%
              pull(.data$program_year)

            if(length(available_years) > 0) {
              most_recent_year <- max(available_years)
              most_recent_marketing_year <- paste0(most_recent_year, "-", most_recent_year + 1)
              if(!quiet) warning(paste0("No effective reference price found for ", crop, " in ", marketing_year, ". Using most recent available year: ", most_recent_marketing_year))

              erp_data = fsaEffectiveRefPrices %>%
                dplyr::filter(.data$crop == .env$crop, .data$marketing_year == .env$most_recent_marketing_year)
            }
          }

          if(!is.null(crop_type)){
            erp_data <- erp_data %>%
              dplyr::filter(.data$crop_type == .env$crop_type)
          }

          erp_values <- erp_data %>% pull(effective_reference_price)

          if(length(erp_values) > 1){
            if(!quiet) warning(paste0("No crop type supplied, taking the average effective reference price across all crop types for ", crop))
            erp <- mean(erp_values, na.rm = TRUE)
          } else if(length(erp_values) == 1) {
            erp <- erp_values[1]
          }
        } else {
          # For program years before 2019, look up SRP as fallback
          data("fsaEffectiveRefPrices", envir = environment())
          srp_data = fsaEffectiveRefPrices %>%
            dplyr::filter(.data$crop == .env$crop, .data$program_year == 2019)

          if(!is.null(crop_type)){
            srp_data <- srp_data %>%
              dplyr::filter(.data$crop_type == .env$crop_type)
          }

          srp_values <- srp_data %>% pull(statutory_reference_price)
          if(length(srp_values) > 0) {
            erp <- ifelse(length(srp_values) > 1, mean(srp_values, na.rm = TRUE), srp_values[1])
          }
        }
      }, error = function(e) {
        if(!quiet) warning("Could not look up ERP for benchmark price adjustment: ", e$message)
      })
    }

    # Apply ERP adjustment if ERP was found
    if(!is.null(erp) && !is.na(erp) && !is.na(benchmark_price) && benchmark_price < erp) {
      if(!quiet) message("Benchmark price (", benchmark_price, ") below ERP (", erp, "). Using ERP value.")
      return(erp)
    }
  }

  return(arcco_data[[column_name]])
}

#' Get ARC-CO Actual Revenue
#'
#' This internal helper function calculates ARC-CO actual revenue by retrieving actual yield
#' from the fsaArcCoBenchmarks dataset and multiplying it by the higher of MYA price or
#' national marketing loan rate (NMLR).
#'
#' @param crop Character. The crop name to filter for (e.g., "corn", "soybeans").
#' @param program_year Numeric. The program year to filter for (e.g., 2024).
#' @param mya_price Numeric. The Marketing Year Average price.
#' @param nmlr Numeric. The national marketing loan rate.
#' @param crop_type Character, optional. The crop type to filter for (e.g., "long grain", "large").
#' @param yield_type Character, optional. The yield type to filter for from the yield_type column.
#' @param state Character, optional. State identifier that can be:
#'   \\itemize{
#'     \\item State abbreviation (e.g., "IA", "IL")
#'     \\item Full state name (e.g., "Iowa", "Illinois")
#'     \\item State FIPS code (e.g., "19", "17")
#'   }
#' @param county Character, optional. County name (requires state to also be specified).
#' @param fips Character, optional. 5-digit FIPS code for specific county-level data.
#' @param quiet Logical. If TRUE, suppresses warning messages (default: FALSE).
#'
#' @return Numeric. The calculated ARC-CO actual revenue (actual_yield × max(mya_price, nmlr)).
#'
#' @details
#' The function follows this calculation:
#' \\enumerate{
#'   \\item Retrieves actual_yield from fsaArcCoBenchmarks dataset
#'   \\item If MYA price >= NMLR: revenue = actual_yield × mya_price
#'   \\item If MYA price < NMLR: revenue = actual_yield × nmlr
#' }
#'
#' Location filtering follows the same priority order as other ARC-CO functions.
#'
#' @examples
#' \dontrun{
#' # County-specific actual revenue
#' get_arcco_actual_revenue("corn", 2024, mya_price = 4.50, nmlr = 4.30, fips = "19001")
#'
#' # State average with crop and yield types
#' get_arcco_actual_revenue("rice", 2024, mya_price = 15.20, nmlr = 14.50,
#'                          crop_type = "long grain", yield_type = "irrigated", state = "IA")
#' }
#'
#' @importFrom utils data
#' @keywords internal
get_arcco_actual_revenue <- function(crop, program_year, mya_price, nmlr, crop_type = NULL, yield_type = NULL, state = NULL, county = NULL, fips = NULL, quiet = FALSE) {
  # Validate inputs
  if(is.null(mya_price) || !is.numeric(mya_price)) {
    stop("mya_price must be a numeric value")
  }
  if(is.null(nmlr) || !is.numeric(nmlr)) {
    stop("nmlr must be a numeric value")
  }

  data("fsaArcCoBenchmarks", envir = environment())

  # filter on crop and marketing year
  target_year <- ifelse(program_year <= 2014, 2014, program_year)
  arcco_data = fsaArcCoBenchmarks %>%
    dplyr::filter(.data$crop == .env$crop,
                  .data$program_year == .env$target_year,
                  .data$actual_yield != "NaN", !is.na(.data$actual_yield))
  
  # If no data found for target year, fall back to most recent available year
  if(nrow(arcco_data) == 0) {
    available_years <- fsaArcCoBenchmarks %>%
      dplyr::filter(.data$crop == .env$crop,
                    .data$actual_yield != "NaN", !is.na(.data$actual_yield)) %>%
      dplyr::pull(.data$program_year) %>%
      unique() %>%
      sort(decreasing = TRUE)
    
    if(length(available_years) > 0) {
      fallback_year <- available_years[1]
      if(!quiet) warning(paste0("No valid actual yield data for ", crop, " in ", target_year, ". Using data from ", fallback_year, "."))
      arcco_data = fsaArcCoBenchmarks %>%
        dplyr::filter(.data$crop == .env$crop,
                      .data$program_year == .env$fallback_year,
                      .data$actual_yield != "NaN", !is.na(.data$actual_yield))
    }
  }

  # filter by crop_type if provided
  if(!is.null(crop_type) && length(crop_type) > 0 && !is.na(crop_type)){
    arcco_data <- arcco_data %>%
      dplyr::filter(.data$crop_type == .env$crop_type)
  }

  # filter by yield_type if provided
  if(!is.null(yield_type)){
    arcco_data <- arcco_data %>%
      dplyr::filter(.data$yield_type == .env$yield_type)
  }

  # filter the data based on location input
  if (!is.null(fips)) {
    # Use fips if provided - clean and validate using clean_fips approach
    fips_clean <- clean_fips(fips = fips)
    arcco_data <- arcco_data %>% dplyr::filter(.data$fips == fips_clean)
    if(!quiet) warning("No actual yield supplied. Using county actual yield for ARC-CO revenue calculations.")
  } else if (!is.null(state)) {
    # Validate state input (accepts names, abbreviations, or fips codes)
    state <- valid_state(state)

    # Convert state abbreviation to full name if needed (since data has full names)
    if (tolower(state) %in% tolower(datasets::state.abb)) {
      # Convert abbreviation to full name
      state_index <- which(tolower(datasets::state.abb) == tolower(state))
      state_name <- datasets::state.name[state_index]
    } else if (tolower(state) %in% tolower(datasets::state.name)) {
      # State is already a full name
      state_name <- state
    } else if (nchar(state) <= 2 && suppressWarnings(!is.na(as.numeric(state)))) {
      # State is a fips code - need to convert to state name
      state_fips <- sprintf("%02d", as.numeric(state))
      # Get state name from fips code
      fips_to_state <- data.frame(
        fips = usmap::fips(state = datasets::state.name),
        name = datasets::state.name
      )
      state_match <- fips_to_state[fips_to_state$fips == state_fips, "name"]
      if (length(state_match) == 0) {
        stop("Invalid state fips code: ", state)
      }
      state_name <- state_match[1]
    } else {
      stop("Invalid state input: ", state)
    }

    if (!is.null(county)) {
      # Both state and county provided - filter by state name and county
      arcco_data <- arcco_data %>% dplyr::filter(.data$state == state_name, .data$county == county)
    } else {
      # Only state provided - filter by state name and average
      arcco_data <- arcco_data %>% dplyr::filter(.data$state == state_name) %>%
        summarize(actual_yield = mean(as.numeric(.data$actual_yield), na.rm = TRUE))
      if(!quiet) warning("No county supplied. Using state average actual yield for ARC-CO revenue calculations.")
    }
  } else {
    # No location filtering - use national average
    if(!quiet) warning("No location parameters supplied. Using national average actual yield for ARC-CO revenue calculations.")
    arcco_data <- arcco_data %>%
      summarize(actual_yield = mean(as.numeric(.data$actual_yield), na.rm = TRUE))
  }

  # Check if we have multiple values and need to warn about averaging across types
  if(nrow(arcco_data) > 1 && "actual_yield" %in% names(arcco_data)){
    if(is.null(crop_type) && is.null(yield_type)){
      if(!quiet) warning(paste0("No crop type or yield type supplied, taking the average actual yield across all types for ", crop))
    } else if(is.null(crop_type)){
      if(!quiet) warning(paste0("No crop type supplied, taking the average actual yield across all crop types for ", crop))
    } else if(is.null(yield_type)){
      if(!quiet) warning(paste0("No yield type supplied, taking the average actual yield across all yield types for ", crop))
    } else {
      if(!quiet) warning(paste0("Multiple actual yield values identified, taking the average actual yield for ", crop))
    }
    # Take the average when multiple rows exist
    arcco_data <- arcco_data %>%
      summarize(actual_yield = mean(as.numeric(.data$actual_yield), na.rm = TRUE))
  }

  # Check if we have any data
  if(nrow(arcco_data) == 0) {
    stop(paste("No valid actual yield data found for crop:", crop))
  }
  
  # Get the actual yield and ensure it's numeric
  actual_yield <- as.numeric(arcco_data$actual_yield)

  # Calculate revenue: actual_yield × max(mya_price, nmlr)
  price_to_use <- ifelse(mya_price >= nmlr, mya_price, nmlr)
  actual_revenue <- actual_yield * price_to_use

  return(actual_revenue)
}

#' Calculate Effective Reference Price (Internal Function)
#'
#' This internal helper function calculates the effective reference price (ERP) using
#' the Olympic average method based on 5 years of Marketing Year Average (MYA) prices
#' and the statutory reference price.
#'
#' @param mya_prices Numeric vector. Five years of MYA prices for Olympic average calculation.
#' @param srp Numeric. The statutory reference price for the crop.
#'
#' @return Numeric. The calculated effective reference price based on the formula:
#'   \\itemize{
#'     \\item If 85% of Olympic average < SRP, then ERP = SRP
#'     \\item If 85% of Olympic average >= 115% of SRP, then ERP = 115% of SRP
#'     \\item Otherwise, ERP = 85% of Olympic average
#'   }
#'
#' @details
#' The Olympic average removes the highest and lowest values from the 5 MYA prices
#' and averages the remaining 3 middle values. The ERP is then calculated as 85%
#' of this Olympic average, subject to minimum and maximum bounds relative to the SRP.
#'
#' @examples
#' \dontrun{
#' # Calculate ERP with 5 years of MYA prices
#' mya_prices <- c(3.70, 3.60, 3.50, 3.40, 3.30)
#' srp <- 3.90
#' calc_effective_reference_price(mya_prices, srp)
#' }
#'
#' @keywords internal
calc_effective_reference_price <- function(mya_prices, srp) {
  # Validate inputs
  if(length(mya_prices) != 5) {
    stop("Exactly 5 MYA prices are required for ERP calculation")
  }
  if(is.null(srp) || !is.numeric(srp)) {
    stop("Statutory reference price (srp) must be a numeric value")
  }
  if(any(is.na(mya_prices))) {
    stop("MYA prices cannot contain NA values. All 5 historic MYA prices must be numeric.")
  }

  # Calculate Olympic average (remove highest and lowest, average the rest)
  sorted_prices <- sort(mya_prices)
  if(length(sorted_prices) >= 3) {
    olympic_avg <- mean(sorted_prices[2:(length(sorted_prices)-1)])
  } else if(length(sorted_prices) > 0) {
    # If fewer than 3 values, just use the mean of all available values
    olympic_avg <- mean(sorted_prices)
  } else {
    stop("No valid MYA prices provided for ERP calculation")
  }

  # Apply ERP formula
  if(0.85 * olympic_avg < srp) {
    erp <- srp
  } else if(0.85 * olympic_avg >= 1.15 * srp) {
    erp <- srp * 1.15
  } else {
    erp <- 0.85 * olympic_avg
  }

  return(erp)
}

#' List asset names from the latest GitHub release
#'
#' Retrieves the metadata for the most recent release of the **rfsa** repository
#' on GitHub and extracts the names of all attached release assets.
#'
#' @return A character vector of file names (assets) in the latest release.
#' @keywords internal
#' @examples
#' \dontrun{
#' files = list_data_assets()
#' }
#' @importFrom gh gh
list_data_assets <- function(){
  # 1. Fetch the release metadata (by tag, or "latest")
  release <- gh::gh(
    "/repos/{owner}/{repo}/releases/latest",
    owner = "dylan-turner25",
    repo  = "rfsa"
  )

  # 2. Extract the assets list
  assets <- release$assets

  # 3. Pull out the bits you care about
  df <- data.frame(
    name = vapply(assets, `[[`, "", "name"),
    url  = vapply(assets, `[[`, "", "browser_download_url"),
    size = vapply(assets, `[[`, 0,  "size"),
    stringsAsFactors = FALSE
  )

  return(df$name)
}

#' Null coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' A helper function that checks to make sure the specified state is valid
#' @noRd
#' @keywords internal
#'
valid_state <- function(state) {
  if (!(F %in% (tolower(state) %in% tolower(datasets::state.name)))) {
    return(state)
  } else if (!(F %in% (tolower(state) %in% tolower(datasets::state.abb)))) {
    return(state)
  } else if (!(F %in% (suppressWarnings(as.numeric(state) %in% as.numeric(usmap::fips(state = datasets::state.name)))))) {
    return(state)
  } else {
    stop("Parameter value for state not valid.")
  }
}

#' @title Download a data file from GitHub Releases via piggyback
#' @param name   The basename of the .rds file, e.g. "foo.rds"
#' @param tag    Which release tag to download from (default: latest)
#' @return       The local path to the downloaded file
#' @keywords internal
#' @noRd
#' @import piggyback
#' @importFrom cli cli_inform
get_cached_rds <- function(name,
                           repo = "dylan-turner25/rfsa",
                           tag  = NULL) {
  dest_dir <- tools::R_user_dir("rfsa", which = "cache")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  dest_file <- file.path(dest_dir, name)
  if (!file.exists(dest_file)) {
    # Only show CLI message if piggyback is not verbose
    show_cli_message <- !isTRUE(getOption("piggyback.verbose", FALSE))

    if (show_cli_message) {
      cli::cli_inform("Downloading {.file {name}} from GitHub...")
    }

    # download from the Release
   piggyback::pb_download(
      file     = name,
      repo     = repo,
      tag      = tag,
      dest = dest_dir
    )

    if (show_cli_message) {
      cli::cli_inform("{.file {name}} downloaded successfully")
    }
  }
  readRDS(dest_file)
}

#' Clear the package cache of downloaded RDS files
#'
#' Deletes the entire cache directory used by the **rfsa** package to store
#' downloaded \*.rds files. Useful if you need to force re-download of data,
#' or free up disk space.
#'
#' @return Invisibly returns `NULL`. A message is printed indicating which
#'   directory was cleared.
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove all cached RDS files so they will be re-downloaded on next use
#' clear_rfsa_cache()
#' }
clear_rfsa_cache <- function(){
  dest_dir <- tools::R_user_dir("rfsa", which = "cache")
  if (dir.exists(dest_dir)) {
    unlink(dest_dir, recursive = TRUE, force = TRUE)
  }
  message("Cleared cached files in ", dest_dir)
  invisible(NULL)
}
#' Splits FSA individual payment files into program/state files
#'
#' @param data a data frame containing FSA payment data
#' @param year_type a character string indicating the type of year to split by.
#'
#' @return splits data into subsets by payment year and FSA program then saves them to the
#' relevant rds file
#' @noRd
#' @keywords internal
#' @import dplyr rlang
#'
split_file <- function(data, year_type){

  # merge duplicate entries into a single transaction
  data <- data.frame(data %>%
    group_by(across(-.data$disbursement_amount)) %>%
    summarise(disbursement_amount = sum(.data$disbursement_amount, na.rm = TRUE),
              .groups = "drop"))

  # convert entire data frame to character
  data <- data %>% mutate(across(everything(), as.character))


  if(year_type == "program"){
    year_var <- "accounting_program_year"
  }
  if(year_type == "payment"){
    year_var <- "payment_year"
  }
  if(year_type == "fiscal"){
    year_var <- "fiscal_year"
  }

  years    <- unique(data[,year_var])
  programs <- unique(data$prog_abb)

  for(y in years){
    for(p in programs){

      # ensure names are portables
      p <- gsub(" ","-",p)

      #print( paste0(year_type," year ",y,"-",p) )
      # make sure folder exists
      dir <- file.path("data-raw","fsaFarmPayments","output_data",
                       paste0(year_type, "_year_files"), y)

      #print(dir)
      if(!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
      }

      file_name <- file.path(dir, paste0(year_type,"_",y,"_",p, ".rds"))
      #print(file_name)

      new_observations <- switch(
        year_type,
        payment = filter(data, .data$prog_abb     == p, .data$payment_year == y),
        fiscal  = filter(data, .data$prog_abb     == p, .data$fiscal_year  == y),
        program = filter(data, .data$prog_abb     == p, .data$accounting_program_year == y),
        stop("Unknown year_type")
      )

      # if file_name does not exist, save new observations as file name
      # otherwise read in existing file and merge with new observations before saving
      if(!file.exists(file_name)){
        saveRDS(new_observations, file_name)
      } else {
        existing_observations <- readRDS(file_name)
        merged_observations   <- dplyr::distinct(bind_rows(existing_observations, new_observations))
        saveRDS(merged_observations, file_name)
      }
    }
  }

}


#' Clean fips codes or construct them from state and county codes
#'
#' If a string is passed to fips, the function will convert the fips codes
#' to 5 digit codes (in the case where 3 or 4 digit codes are present)
#'
#' If state and county fips codes are passed to the county and state arguemnts,
#' a 5 digit fips code will be constructed
#'
#' @param fips a character vector representing a fips codes
#' @param county a character vector representing a county fips code
#' @param state a character vector representing a state fips code
#'
#' @return returns a character vector containing 5 digit fips codes
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' clean_fips(fips = "1001")
#' clean_fips(county = "1", state = "1")
#'
clean_fips <- Vectorize(function(fips = NULL,county = NULL,state = NULL){


  if(is.null(fips)){
    if(nchar(state) == 1){
      state_clean <- paste0("0",state)
    }
    if(nchar(state) == 2){
      state_clean <- as.character(state)
    }
    if(nchar(county) == 1){
      county_clean <- paste0("00",county)
    }
    if(nchar(county) == 2){
      county_clean <- paste0("0",county)
    }
    if(nchar(county) == 3){
      county_clean <- as.character(county)
    }
    fips <- paste0(state_clean,county_clean)
    return(fips)
  }
  if(is.null(fips) == F){
    if(nchar(fips) == 4){
      fips_clean <- paste0("0",fips)
    }
    if(nchar(fips) < 4){
      print("Warning: fips codes should be either 4 or 5 character strings. Returning NA")
      return(NA)
    }
    if(nchar(fips) == 5){
      fips_clean <- as.character(fips)
    }
    return(fips_clean)
  }
})

#' Clean and standardize column names for FSA MYA Price files
#'
#' Takes a data frame of FSA "MYA Price" data and replaces fiscal-year patterns
#' and other inconsistent suffixes with a uniform T-minus scheme, plus standardizes
#' "Prices" to singular "Price" and final/projection markers.
#'
#' @param df A data.frame containing FSA MYA Price columns named with patterns like "2014/15 Prices".
#' @param year Integer. The calendar year corresponding to the most recent data in `df`;
#'   used to generate T-minus replacements back to `year - 10`.
#'
#' @return A character vector of cleaned column names.
#' @noRd
#' @keywords internal
rename_mya_cols <- function(df, year) {
  replacement <- 0
  for (y in seq(year, year - 10)) {
    pattern <- paste0(y, "/", substr(y + 1, 3, 4))
    colnames(df) <- gsub(pattern, paste0("T - ", replacement), colnames(df))
    replacement <- replacement + 1
  }
  colnames(df) <- gsub("Prices", "Price", colnames(df))
  colnames(df) <- gsub("Projected \\(P\\) or Final \\(F\\)", "Final", colnames(df))
  colnames(df) <- gsub("Projected \\(P\\) Final \\(F\\)", "Final", colnames(df))

  return(colnames(df))
}

#' Extract type of a crop from its name
#'
#' Given a human-readable crop name string, returns the grain type
#' (e.g. "long grain", "short/medium grain") or cotton seed indicator.
#'
#' @param crop_name Character. The commodity name as found in FSA or RMA outputs.
#' @param rma_code Logical. If `TRUE`, returns the RMA internal code string;
#'   otherwise returns a descriptive label.
#'
#' @return Character or `NA`. If `rma_code = FALSE`, one of "long grain",
#'   "short/medium grain", "temperate japonica", "seed", "large", "small";
#'   if `rma_code = TRUE`, the corresponding RMA code string (e.g. "453", "451/452");
#'   returns `NA` if no match.
#' @noRd
#' @keywords internal
extract_crop_type <- function(crop_name, rma_code = FALSE) {

  # cotton seed vs lint not coded
  if (grepl("cotton", tolower(crop_name))) {
    if (grepl("seed", tolower(crop_name))) {
      return(if (rma_code) NA_character_ else "seed")
    }
    if (grepl("upland", tolower(crop_name))) {
      return(if (rma_code) NA_character_ else "upland")
    }
  }

  # rice types
  if (grepl("rice", tolower(crop_name))) {
    if (grepl("long", tolower(crop_name))) {
      return(if (rma_code) "453" else "long grain")
    } else if (grepl("medium|short|med", tolower(crop_name))) {
      return(if (rma_code) "451/452" else "short/medium grain")
    } else if (grepl("japonica|temp", tolower(crop_name))) {
      return(if (rma_code) "451/452" else "temperate japonica")
    }
  }

  # chickpeas
  if (grepl("chickpea", tolower(crop_name))) {
    if (rma_code) return(NA_character_)
    if (grepl("large", tolower(crop_name))) {
      return("large")
    } else if (grepl("small", tolower(crop_name))) {
      return("small")
    }
  }

  # no match
  return(NA_character_)
}

#' Standardize free-text crop names
#'
#' Remove footnote indicators, parentheses, size qualifiers, and trim whitespace.
#'
#' @param crop_name Character. The raw crop name possibly containing notes
#'   or parentheses.
#'
#' @return Character. The cleaned crop name in lowercase without annotations.
#' @noRd
#' @keywords internal
clean_crop_names <- function(crop_name) {
  # Remove footnote patterns: numbers followed by slash, space+slash+numbers, slash+numbers
  cleaned_name <- gsub("[0-9]+/|\\s*/[0-9]+|/[0-9]+", "", crop_name)
  # Remove standalone numbers (footnote indicators) at the end
  cleaned_name <- gsub("\\s+[0-9]+$", "", cleaned_name)
  cleaned_name <- tolower(cleaned_name)
  cleaned_name <- gsub("\\s*\\([^)]*\\)", "", cleaned_name)
  cleaned_name <- gsub("\\blarge\\b|\\bsmall\\b|\\bseed\\b|\\bupland\\b", "", cleaned_name)
  cleaned_name <- trimws(cleaned_name)
  return(cleaned_name)
}


#' An upgraded version of `clean_crop_names` that can handle weird abbreviations
#'
#' Remove footnote indicators, parentheses, size qualifiers, and trim whitespace.
#' Also ccorrects abbreviated names to their full form.
#'
#' @param crop_name Character. The raw crop name possibly containing notes
#'   or parentheses.
#'
#' @return Character. The cleaned crop name in lowercase without annotations.
#' @noRd
#' @keywords internal
clean_crop_names2 <- function(crop_name) {
  if (is.na(crop_name)) return(NA_character_)

  # Remove numeric patterns and slash
  cleaned_name <- gsub("[0-9]/", "", crop_name)

  # Convert to lowercase
  cleaned_name <- tolower(cleaned_name)

  # Remove text in parentheses
  cleaned_name <- gsub("\\s*\\([^)]*\\)", "", cleaned_name)

  # Remove specific words
  cleaned_name <- gsub("\\blarge\\b|\\bsmall\\b|\\bseed\\b|\\bupland\\b", "", cleaned_name)

  # Additional cleaning
  cleaned_name <- gsub(" - blank$", "", cleaned_name)
  cleaned_name <- gsub(" - .*grain$", "", cleaned_name)
  cleaned_name <- gsub(" - temporate japonica$", "", cleaned_name)
  cleaned_name <- gsub(" - garbanzo-lg kabuli$| - garbanzo-sm desi$", "", cleaned_name)

  # Remove common crop type suffixes
  cleaned_name <- gsub("\\s+lint$", "", cleaned_name)  # "cotton lint" -> "cotton"
  cleaned_name <- gsub("\\bmed/short grain$|\\blong grain$|\\bmedium grain$|\\bshort grain$", "", cleaned_name)
  cleaned_name <- gsub("\\btemperate japonica$|\\btropical japonica$", "", cleaned_name)
  cleaned_name <- gsub("\\blarge kabuli$|\\bsmall desi$", "", cleaned_name)
  cleaned_name <- gsub("\\boil type$|\\bconfection type$", "", cleaned_name)
  cleaned_name <- gsub("\\bdark northern spring$|\\bhard red winter$|\\bsoft red winter$|\\bwhite$|\\bdurum$", "", cleaned_name)

  # Remove underscore-separated type information (e.g., "chickpeas_large" -> "chickpeas")
  cleaned_name <- gsub("_.*$", "", cleaned_name)

  # Remove specific rice type patterns that might remain
  cleaned_name <- gsub("\\s+(med/.*|long\\s+grain|temperate\\s+japonica)$", "", cleaned_name)

  # Handle "beans- chickpeas" -> "chickpeas" (this specific case before hyphen removal)
  if(grepl("beans.*chickpea", cleaned_name)) {
    cleaned_name <- "chickpeas"
  } else {
    # Remove hyphen-separated type information (e.g., "rice-med grain" -> "rice")
    cleaned_name <- gsub("-.*$", "", cleaned_name)
  }

  # Standardize abbreviated names
  lookup <- c(
    "barly" = "barley",
    "pnuts" = "peanuts",
    "snflr" = "sunflowers",
    "sflwr" = "safflower",
    "sorgh" = "sorghum",
    "soybn" = "soybeans",
    "canol" = "canola",
    "sesme" = "sesame",
    "lenti" = "lentils",
    "mustd" = "mustard",
    "rape" = "rapeseed",
    "cramb" = "crambe"
  )

  for (abbr in names(lookup)) {
    if (cleaned_name == abbr) {
      cleaned_name <- lookup[abbr]
      break
    }
  }

  # Final trim of whitespace
  cleaned_name <- trimws(cleaned_name)

  # Remove trailing hyphens (fixes cases like "cotton-" from "seed cotton-upland")
  cleaned_name <- gsub("-+$", "", cleaned_name)

  return(cleaned_name)
}

#' Assign RMA commodity codes to crop names
#'
#' Maps a crop name string to its RMA numeric commodity code.
#'
#' @param crop_name Character. The commodity name to match.
#'
#' @return Integer or `NA`. The RMA commodity code (e.g. 41, 81, 11, etc.),
#'   or `NA` if the crop is not in the predefined mapping.
#' @noRd
#' @keywords internal
assign_rma_cc <- function(crop_name) {
  name_lc <- tolower(crop_name)
  if (grepl("corn", name_lc)) {
    return(41)
  } else if (grepl("soybean", name_lc)) {
    return(81)
  } else if (grepl("wheat", name_lc)) {
    return(11)
  } else if (grepl("rice", name_lc)) {
    return(18)
  } else if (grepl("cotton", name_lc)) {
    return(21)
  } else if (grepl("sorghum", name_lc)) {
    return(51)
  } else if (grepl("barley", name_lc)) {
    return(91)
  } else if (grepl("oat", name_lc)) {
    return(16)
  } else if (grepl("peanut", name_lc)) {
    return(75)
  } else if (grepl("canola|rape", name_lc)) {
    return(15)
  } else if (grepl("sunflower", name_lc)) {
    return(78)
  } else if (grepl("dry pea|chickpea|lentil", name_lc)) {
    return(67)
  } else if (grepl("flaxseed|linseed", name_lc)) {
    return(31)
  } else if (grepl("mustard seed|mustard", name_lc)) {
    return(69)
  } else if (grepl("safflower", name_lc)) {
    return(49)
  } else if (grepl("potato|sweet potato", name_lc)) {
    return(156)
  } else if (grepl("sesame", name_lc)) {
    return(396)
  } else {
    return(NA_integer_)
  }
}




#' Convert POSIX date object to U.S. Government fiscal year
#'
#' @param date A calendar date formatted as a posix object
#'
#' @return returns a numeric value for the Government fiscal year corresponding
#' to the POSIX date
#' @noRd
#' @keywords internal
#'
get_fiscal_year <- function(date){

  # extract the calendar year from the date input
  year <- as.numeric(format(date, format="%Y"))

  # if date is between october 1st of last year and september 30th of this year
  # return the current calendar year
  if(date >=  as.Date(paste0((year - 1),"-10-01"),"%Y-%m-%d") &
     date <=   as.Date(paste0((year),"-09-30"),"%Y-%m-%d")) {
    return(year)
  }

  # if date is between october 1st of this calendar year and september 30th of
  # next calender year, return the current calendar year + 1
  if(date >=  as.Date(paste0((year),"-10-01"),"%Y-%m-%d") &
     date <=   as.Date(paste0((year + 1),"-09-30"),"%Y-%m-%d")) {
    return(year + 1)
  }

}

#' Vectorized ARC-CO Benchmarks Calculation (Batch Processing)
#'
#' This function calculates ARC-CO benchmark yields or prices for multiple observations
#' in a vectorized manner, providing significant performance improvements over individual
#' calls to get_arcco_benchmarks().
#'
#' @param crop Character vector. The crop names.
#' @param program_year Numeric vector. The program years.
#' @param benchmark_type Character. Either "yield" or "price".
#' @param crop_type Character vector or NULL. Crop types (optional).
#' @param yield_type Character vector or NULL. Yield types (optional).
#' @param historical_data Matrix or NULL. For yield: matrix with 5 columns of historical yields.
#'   For price: matrix with 5 columns of historical prices.
#' @param erp Numeric vector or NULL. Effective reference prices (for price calculations).
#' @param fips Character vector or NULL. FIPS codes for location-specific data.
#' @param quiet Logical. If TRUE, suppresses messages.
#'
#' @return Numeric vector. The calculated benchmark values.
#'
#' @keywords internal
get_arcco_benchmarks_batch <- function(crop, program_year, benchmark_type = "yield",
                                      crop_type = NULL, yield_type = NULL,
                                      historical_data = NULL, erp = NULL,
                                      fips = NULL, quiet = FALSE) {
  
  n_rows <- length(crop)
  
  # Validate inputs
  if(!benchmark_type %in% c("yield", "price")) {
    stop("benchmark_type must be either 'yield' or 'price'")
  }
  
  # Initialize result vector
  results <- numeric(n_rows)
  
  # Handle historical data mode (Olympic averages)
  if(!is.null(historical_data)) {
    if(ncol(historical_data) != 5) {
      stop("historical_data must have exactly 5 columns")
    }
    
    for(i in seq_len(n_rows)) {
      hist_values <- historical_data[i, ]
      
      # Handle NA values - if all NA, return NA
      if(all(is.na(hist_values))) {
        results[i] <- NA_real_
        next
      }
      
      # For price calculations, apply ERP adjustment
      if(benchmark_type == "price" && !is.null(erp)) {
        erp_val <- erp[i]
        if(!is.na(erp_val)) {
          # Replace any price below ERP with ERP
          hist_values[!is.na(hist_values) & hist_values < erp_val] <- erp_val
        }
      }
      
      # Calculate Olympic average (remove highest and lowest)
      if(sum(!is.na(hist_values)) >= 3) {
        sorted_values <- sort(hist_values[!is.na(hist_values)])
        if(length(sorted_values) >= 3) {
          olympic_avg <- mean(sorted_values[2:(length(sorted_values)-1)])
          results[i] <- olympic_avg
        } else {
          results[i] <- mean(sorted_values, na.rm = TRUE)
        }
      } else {
        results[i] <- mean(hist_values, na.rm = TRUE)
      }
    }
    
    return(results)
  }
  
  # FSA Data lookup mode - batch process
  data("fsaArcCoBenchmarks", envir = environment())
  
  # Determine column name
  column_name <- ifelse(benchmark_type == "yield", "oa_bench_mark_yield", "oa_bench_mark_price")
  
  # Create lookup data frame for batch processing
  lookup_df <- data.frame(
    idx = seq_len(n_rows),
    crop = crop,
    program_year = pmax(program_year, 2014), # Default to 2014 for earlier years
    crop_type = crop_type %||% rep(NA_character_, n_rows),
    yield_type = yield_type %||% rep(NA_character_, n_rows),
    fips = fips %||% rep(NA_character_, n_rows),
    stringsAsFactors = FALSE
  )
  
  # Pre-process ARC-CO data
  arcco_processed <- fsaArcCoBenchmarks
  
  # Batch lookup using joins
  for(i in seq_len(n_rows)) {
    row_data <- lookup_df[i, ]
    
    # Filter ARC-CO data for this observation
    filtered_data <- arcco_processed %>%
      dplyr::filter(.data$crop == row_data$crop,
                    .data$program_year == row_data$program_year)
    
    # Apply optional filters
    if(!is.na(row_data$crop_type)) {
      filtered_data <- filtered_data %>%
        dplyr::filter(.data$crop_type == row_data$crop_type)
    }
    
    if(!is.na(row_data$yield_type)) {
      filtered_data <- filtered_data %>%
        dplyr::filter(.data$yield_type == row_data$yield_type)
    }
    
    # Apply location filter
    if(!is.na(row_data$fips) && row_data$fips != "") {
      fips_clean <- clean_fips(row_data$fips)
      filtered_data <- filtered_data %>%
        dplyr::filter(.data$fips == fips_clean)
    }
    
    # Extract the value
    if(nrow(filtered_data) > 0) {
      value <- filtered_data[[column_name]]
      if(length(value) > 1) {
        if(!quiet) warning(paste("Multiple", benchmark_type, "values found for", row_data$crop, "- taking average"))
        results[i] <- mean(value, na.rm = TRUE)
      } else {
        results[i] <- value[1]
      }
    } else {
      if(!quiet) warning(paste("No", benchmark_type, "data found for", row_data$crop))
      results[i] <- NA_real_
    }
  }
  
  return(results)
}

#' Vectorized ARC-CO Actual Revenue Calculation (Batch Processing)
#'
#' This function calculates ARC-CO actual revenue for multiple observations
#' in a vectorized manner, providing significant performance improvements.
#'
#' @param crop Character vector. The crop names.
#' @param program_year Numeric vector. The program years.
#' @param mya_price Numeric vector. Marketing Year Average prices.
#' @param nmlr Numeric vector. National Marketing Loan Rates.
#' @param crop_type Character vector or NULL. Crop types (optional).
#' @param yield_type Character vector or NULL. Yield types (optional).
#' @param fips Character vector or NULL. FIPS codes for location-specific data.
#' @param quiet Logical. If TRUE, suppresses messages.
#'
#' @return Numeric vector. The calculated actual revenue values.
#'
#' @keywords internal
get_arcco_actual_revenue_batch <- function(crop, program_year, mya_price, nmlr,
                                          crop_type = NULL, yield_type = NULL,
                                          fips = NULL, quiet = FALSE) {
  
  n_rows <- length(crop)
  results <- numeric(n_rows)
  
  # Load ARC-CO data once
  data("fsaArcCoBenchmarks", envir = environment())
  
  # Vectorize the price comparison (actual yield × max(mya_price, nmlr))
  price_to_use <- pmax(mya_price, nmlr, na.rm = TRUE)
  
  # Batch lookup actual yields
  lookup_df <- data.frame(
    idx = seq_len(n_rows),
    crop = crop,
    program_year = program_year,
    crop_type = crop_type %||% rep(NA_character_, n_rows),
    yield_type = yield_type %||% rep(NA_character_, n_rows),
    fips = fips %||% rep(NA_character_, n_rows),
    stringsAsFactors = FALSE
  )
  
  # Process in batches for efficiency
  for(i in seq_len(n_rows)) {
    row_data <- lookup_df[i, ]
    
    # Apply fallback logic for missing data
    target_year <- row_data$program_year
    fallback_years <- target_year - 1
    
    actual_yield <- NA_real_
    
    # Try current year first
    for(year_to_try in c(target_year, fallback_years)) {
      filtered_data <- fsaArcCoBenchmarks %>%
        dplyr::filter(.data$crop == row_data$crop,
                      .data$program_year == year_to_try)
      
      # Apply optional filters
      if(!is.na(row_data$crop_type)) {
        filtered_data <- filtered_data %>%
          dplyr::filter(.data$crop_type == row_data$crop_type)
      }
      
      if(!is.na(row_data$yield_type)) {
        filtered_data <- filtered_data %>%
          dplyr::filter(.data$yield_type == row_data$yield_type)
      }
      
      # Apply location filter
      if(!is.na(row_data$fips) && row_data$fips != "") {
        fips_clean <- clean_fips(row_data$fips)
        filtered_data <- filtered_data %>%
          dplyr::filter(.data$fips == fips_clean)
      }
      
      # Get actual yield
      if(nrow(filtered_data) > 0 && "actual_yield" %in% names(filtered_data)) {
        yield_values <- as.numeric(filtered_data$actual_yield)
        yield_values <- yield_values[!is.na(yield_values)]
        
        if(length(yield_values) > 0) {
          actual_yield <- mean(yield_values)
          if(year_to_try != target_year && !quiet) {
            warning(paste("No valid actual yield data for", row_data$crop, "in", target_year, ". Using data from", year_to_try, "."))
          }
          break
        }
      }
    }
    
    # Calculate revenue
    if(!is.na(actual_yield)) {
      results[i] <- actual_yield * price_to_use[i]
    } else {
      if(!quiet) warning(paste("No actual yield data found for", row_data$crop))
      results[i] <- NA_real_
    }
  }
  
  return(results)
}




