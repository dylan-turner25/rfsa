data("fsaArcCoBenchmarks")
data("fsaMyaPrice")
data("fsaPlcYields")
data("fsaEffectiveRefPrices")
data("fsaArcCoPrice")
data("fsaCountyBaseAcres")
data("fsaEnrolledCountyBaseAcres")
data("fsaCountyBaseAcres")
data("fsaCropAcreageCC")
library(rnassqs)


data <- fsaArcCoBenchmarks %>%
  mutate(marketing_year = paste0(program_year, "-", program_year + 1))

# get a data frame of unique fips observations
unique_fips <- data %>%
  select(fips, state_name, county_name) %>%
  distinct()

unique_crops <- data %>%
  select(crop, crop_type) %>%
  distinct()

unique_years <- data %>%
  select(program_year) %>%
  distinct()

# create a data frame that is every combination of unique fips, crop, yield_type, and year
data <- tidyr::crossing(unique_fips, unique_crops, unique_years)

# start with arc co benchmarks
benchmarks <- fsaArcCoBenchmarks %>%
  mutate(marketing_year = paste0(program_year, "-", program_year + 1))

data <- left_join(data, benchmarks)


# merge in plc yield
plc_yields <- fsaPlcYields %>%
  select(fips, crop, crop_type, plc_yield, program_year)
data <- left_join(data, plc_yields)


# merge in arc-co prices
arc_co_price <- fsaArcCoPrice %>%
  select(
    crop,
    contains("benchmark_price"),
    current_mya_price,
    current_national_loan_rate,
    marketing_year,
    program_year,
    crop_type
  )
data <- left_join(data, arc_co_price)

# merge in mya prices
prices <- fsaMyaPrice %>%
  select(
    crop,
    crop_type,
    marketing_year,
    contains("mya_price"),
    -contains("publishing")
  )
data <- left_join(data %>% select(-current_mya_price), prices)


# add statutory_reference_prices
srp <- distinct(
  fsaEffectiveRefPrices %>%
    select(statutory_reference_price, crop, crop_type)
)
data <- left_join(data, srp)

# add observed effective reference prices
erp <- distinct(
  fsaEffectiveRefPrices %>%
    select(
      effective_reference_price,
      crop,
      crop_type,
      marketing_year,
      program_year
    )
)
data <- left_join(data, erp)

# add calculated effective reference prices (as a check)
data$erp_calc <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch(
    {
      # Extract MYA prices for this row
      mya_prices <- c(
        data$final_mya_price_lag2[i],
        data$final_mya_price_lag3[i],
        data$final_mya_price_lag4[i],
        data$final_mya_price_lag5[i],
        data$final_mya_price_lag6[i]
      )

      # Calculate ERP
      rfsa:::calc_effective_reference_price(
        mya_prices = mya_prices,
        srp = data$statutory_reference_price[i]
      )
    },
    error = function(e) {
      # Return NA on any error
      return(NA)
    }
  )
}))

data$erp_calc_check <- as.numeric(
  data$erp_calc == data$effective_reference_price
)
summary(data$erp_calc_check)

# add missing effective reference prices using calculated values
data$effective_reference_price <- ifelse(
  is.na(data$effective_reference_price),
  data$erp_calc,
  data$effective_reference_price
)


# add calculated benchmark price (vectorized with lapply)
data$oa_bench_mark_price_calc <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch(
    {
      # Extract historical benchmark prices for this row
      historical_prices <- c(
        data$annual_benchmark_price_lag1[i],
        data$annual_benchmark_price_lag2[i],
        data$annual_benchmark_price_lag3[i],
        data$annual_benchmark_price_lag4[i],
        data$annual_benchmark_price_lag5[i]
      )

      # Calculate Olympic average benchmark price
      result <- rfsa:::get_arcco_benchmarks(
        crop = data$crop[i],
        program_year = data$program_year[i],
        benchmark_type = "price",
        erp = max(
          data$effective_reference_price[i],
          data$statutory_reference_price[i],
          na.rm = T
        ),
        crop_type = data$crop_type[i],
        historical_prices = historical_prices,
        fips = data$fips[i],
        quiet = TRUE
      )

      return(round(result, 2))
    },
    error = function(e) {
      # Return NA on any error
      return(NA)
    }
  )
}))

# add missing benchmark prices using calculated values
data$oa_bench_mark_price <- ifelse(
  is.na(data$oa_bench_mark_price),
  data$oa_bench_mark_price_calc,
  data$oa_bench_mark_price
)

# merge in total base acres
base <- fsaCountyBaseAcres %>%
  group_by(fips, crop, crop_type,program_year) %>%
  summarize(base_acres = sum(base_acres), .groups = "drop")

data <- left_join(data, base)

# for missing base acres, fill in with the most recent base acres available for that county/crop/crop_type
data <- data %>%
  arrange(fips, crop, crop_type, program_year) %>%
  group_by(fips, crop, crop_type) %>%
  mutate(
    base_acres = zoo::na.locf(base_acres, na.rm = FALSE)
  ) %>%
  ungroup()


# merge in enrolled base acres (current year)
enrolled_base <- fsaEnrolledCountyBaseAcres %>%
  select(fips, program_year, crop, crop_type, contains("enrolled"))

data <- left_join(data, enrolled_base)


# merge in planted acres (assume all cotton acres are seed cotton)
planted_acres <- fsaCropAcreageCC %>%
  mutate(
    crop_type = gsub("upland|extra long staple", "seed", crop_type)
  )

# Create detailed irrigation summary by year, crop, crop_type, and county
irrigation_summary <- planted_acres %>%
  group_by(crop_yr, crop, crop_type, fips, irrigation_practice) %>%
  summarize(
    irrigation_acres = sum(planted_and_failed_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = irrigation_practice,
    values_from = irrigation_acres,
    values_fill = 0
  ) %>%
  mutate(
    total_acres = rowSums(
      select(., -c(crop_yr, crop, crop_type, fips)),
      na.rm = TRUE
    ),
    planted_irrigated_share = round(I / total_acres, 2),
    planted_non_irrigated_share = round(N / total_acres, 2)
  ) %>%
  filter(total_acres > 0)

# merge the irrigation summary into main data
data <- left_join(
  data %>% mutate(fips = as.numeric(fips)),
  irrigation_summary %>%
    select(
      crop_yr,
      crop,
      crop_type,
      fips,
      planted_irrigated_share,
      planted_non_irrigated_share
    ),
  by = c("program_year" = "crop_yr", "crop", "crop_type", "fips")
)

irrigation_summary_crop <- planted_acres %>%
  group_by(crop_yr, crop, crop_type, irrigation_practice) %>%
  summarize(
    irrigation_acres = sum(planted_and_failed_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = irrigation_practice,
    values_from = irrigation_acres,
    values_fill = 0
  ) %>%
  mutate(
    total_acres = rowSums(
      select(., -c(crop_yr, crop, crop_type)),
      na.rm = TRUE
    ),
    planted_irrigated_share_national = round(I / total_acres, 2),
    planted_non_irrigated_share_national = round(N / total_acres, 2)
  ) %>%
  filter(total_acres > 0)

# merge in national level irrigation shares
data <- left_join(
  data,
  irrigation_summary_crop %>%
    select(
      crop_yr,
      crop,
      crop_type,
      planted_irrigated_share_national,
      planted_non_irrigated_share_national
    ),
  by = c("program_year" = "crop_yr", "crop", "crop_type")
)

# use the national level shares if county level shares are missing
data <- data %>%
  mutate(
    planted_irrigated_share = ifelse(
      is.na(planted_irrigated_share),
      planted_irrigated_share_national,
      planted_irrigated_share
    ),
    planted_non_irrigated_share = ifelse(
      is.na(planted_non_irrigated_share),
      planted_non_irrigated_share_national,
      planted_non_irrigated_share
    )
  )

# adjust base acres by irrigation status (vectorized)
data <- data %>%
  mutate(
    base_acres = case_when(
      yield_type == "Nonirrigated" ~ base_acres * planted_non_irrigated_share,
      yield_type == "Irrigated" ~ base_acres * planted_irrigated_share,
      TRUE ~ base_acres
    ),
    enrolled_base_ARCCO = case_when(
      yield_type == "Nonirrigated" ~
        enrolled_base_ARCCO * planted_non_irrigated_share,
      yield_type == "Irrigated" ~ enrolled_base_ARCCO * planted_irrigated_share,
      TRUE ~ enrolled_base_ARCCO
    ),
    enrolled_base_PLC = case_when(
      yield_type == "Nonirrigated" ~
        enrolled_base_PLC * planted_non_irrigated_share,
      yield_type == "Irrigated" ~ enrolled_base_PLC * planted_irrigated_share,
      TRUE ~ enrolled_base_PLC
    )
  )




# Transform planted_acres to match data structure with yield_type
planted_acres_harmonized <- planted_acres %>%
  # First transform irrigation_practice to yield_type
  mutate(
    yield_type = case_when(
      irrigation_practice == "I" ~ "Irrigated",
      irrigation_practice == "N" ~ "Nonirrigated",
      TRUE ~ irrigation_practice
    )
  ) %>%
  select(
    crop_yr,
    crop,
    crop_type,
    fips,
    yield_type,
    planted_and_failed_acres,
    prevented_acres,
    planted_acres,
    failed_acres,
  ) %>%
  # Create "All" category by aggregating irrigated + non-irrigated
  bind_rows(
    planted_acres %>%
      group_by(crop_yr, crop, crop_type, fips) %>%
      summarize(
        planted_and_failed_acres = sum(planted_and_failed_acres, na.rm = TRUE),
        prevented_acres = sum(prevented_acres, na.rm = TRUE),
        planted_acres = sum(planted_acres, na.rm = TRUE),
        failed_acres = sum(failed_acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(yield_type = "All")
  )


planted_acres_harmonized %>%
  filter(crop == "peanuts") %>%
  filter(yield_type == "All") %>%
  group_by(crop_yr) %>%
  summarize(
    planted_acres = sum(planted_acres, na.rm = TRUE)
  )



# Merge in planted acres with yield_type matching
data <- left_join(
  data,
  planted_acres_harmonized,
  by = c("program_year" = "crop_yr", "crop", "crop_type", "fips", "yield_type")
)



# Base acre data, including total base acres and enrolled base acres are adjusted in accordance
# with the share of planted acres that are irrigated vs non-irrigated in each county for each crop.
# In cases where no planted acres data are avaliable (i.e. planted acres are zero for the county and crop),
# the national level share of irrigated vs non-irrigated acres for that crop is used to adjust base acres proportionally.


# if no planted acres are reported, set planted acres to zero
data$planted_acres[is.na(data$planted_acres)] <- 0


# fill in missing current loan rates with previous year's value
data <- data %>%
  arrange(fips, crop, crop_type, program_year) %>%
  group_by(fips, crop, crop_type) %>%
  mutate(
    current_national_loan_rate = zoo::na.locf(
      current_national_loan_rate,
      na.rm = FALSE
    )
  ) %>%
  ungroup()

# NASS API Authentication and Yield Data Collection ========================
nassqs_auth(key = "B26AB9B0-0ED7-3EB9-8FB6-CD0EFAB3D15B")

# Define commodity mapping for NASS API calls
nass_commodity_mapping <- list(
  corn = "CORN",
  soybeans = "SOYBEANS",
  wheat = "WHEAT",
  barley = "BARLEY",
  "grain sorghum" = "SORGHUM",
  rice = "RICE",
  peanuts = "PEANUTS",
  cotton = "COTTON",
  oats = "OATS"
)

#' Get National NASS Yields
#'
#' Collects national-level NASS yield data for all commodities
#'
#' @param years vector of years to collect data for
#' @param commodity_mapping list mapping crop names to NASS commodity descriptions
#' @return dataframe with national NASS yields by commodity and year
get_nass_yields_national <- function(years = 2014:2025, commodity_mapping = nass_commodity_mapping) {
  cat("Collecting national NASS yield data...\n")

  all_nass_national <- data.frame()

  for (crop_name in names(commodity_mapping)) {
    commodities <- commodity_mapping[[crop_name]]

    for (commodity in commodities) {
      cat("  Processing", commodity, "for", crop_name, "\n")

      tryCatch({
        yields <- nassqs_yields(
          commodity_desc = commodity,
          year = years,
          agg_level_desc = "NATIONAL"
        ) %>%
          filter(source_desc == "SURVEY") %>%
          # Find most recent load_time for each year
          group_by(year) %>%
          filter(load_time == max(load_time, na.rm = TRUE)) %>%
          ungroup() %>%
          # Average if multiple records per year
          group_by(year, short_desc) %>%
          summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
          mutate(
            crop = crop_name,
            commodity_desc = commodity,
            nass_yield_national = Value
          )

        if(commodity == "COTTON"){
          yields <- yields %>%
            filter(grepl("upland", tolower(short_desc)))
        }

        yields <- yields %>%
          select(crop, commodity_desc, year, nass_yield_national,short_desc) %>%
          group_by(crop, year,short_desc) %>%
          summarise(nass_yield_national = mean(nass_yield_national, na.rm = TRUE), .groups = 'drop')

        all_nass_national <- bind_rows(all_nass_national, yields)

      }, error = function(e) {
        cat("    Warning: Could not retrieve", commodity, "data:", e$message, "\n")
      })
    }
  }

  cat("National NASS yield collection complete\n")
  return(all_nass_national)
}

#' Get State NASS Yields
#'
#' Collects state-level NASS yield data for all commodities
#'
#' @param years vector of years to collect data for
#' @param commodity_mapping list mapping crop names to NASS commodity descriptions
#' @return dataframe with state NASS yields by commodity, state, and year
get_nass_yields_state <- function(years = 2014:2025, commodity_mapping = nass_commodity_mapping) {
  cat("Collecting state NASS yield data...\n")

  all_nass_state <- data.frame()

  for (crop_name in names(commodity_mapping)) {
    commodities <- commodity_mapping[[crop_name]]

    for (commodity in commodities) {
      cat("  Processing", commodity, "for", crop_name, "\n")

      tryCatch({
        yields <- nassqs_yields(
          commodity_desc = commodity,
          year = years,
          agg_level_desc = "STATE"
        ) %>%
          filter(source_desc == "SURVEY") %>%
          # Find most recent load_time for each year and state
          group_by(year, state_name) %>%
          filter(load_time == max(load_time, na.rm = TRUE)) %>%
          ungroup() %>%
          # Average if multiple records per year/state
          group_by(year, state_name, short_desc) %>%
          summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
          mutate(
            crop = crop_name,
            commodity_desc = commodity,
            nass_yield_state = Value
          )

        if(commodity == "COTTON"){
          yields <- yields %>%
            filter(grepl("upland", tolower(short_desc)))
        }

        yields <- yields %>%
          select(crop, commodity_desc, state_name, year, nass_yield_state, short_desc) %>%
          group_by(crop, state_name, year, short_desc) %>%
          summarise(nass_yield_state = mean(nass_yield_state, na.rm = TRUE), .groups = 'drop')

        all_nass_state <- bind_rows(all_nass_state, yields)

      }, error = function(e) {
        cat("    Warning: Could not retrieve", commodity, "state data:", e$message, "\n")
      })
    }
  }

  cat("State NASS yield collection complete\n")
  return(all_nass_state)
}

# Collect NASS yield data
cat("Starting NASS yield data collection...\n")
nass_yields_national <- get_nass_yields_national() %>%
  mutate(year = as.integer(year)) %>%
  filter(!grepl("silage", tolower(short_desc))) %>%
  group_by(crop, year) %>%
  summarise(nass_yield_national = mean(nass_yield_national, na.rm = TRUE), .groups = 'drop')

nass_yields_state <- get_nass_yields_state() %>%
  mutate(year = as.integer(year)) %>%
  filter(!grepl("silage", tolower(short_desc))) %>%
  group_by(crop, state_name, year) %>%
  summarise(nass_yield_state = mean(nass_yield_state, na.rm = TRUE), .groups = 'drop')

# For national yields - join by crop and program_year
data <- data %>%
  left_join(
    nass_yields_national %>%
      # Handle cotton special case - for cotton, use average of upland and pima
      group_by(crop, year) %>%
      summarise(nass_yield_national = mean(nass_yield_national, na.rm = TRUE), .groups = 'drop'),
    by = c("crop", "program_year" = "year")
  )

# For state yields - join by crop, state_name, and program_year
data <- data %>%
  left_join(
    nass_yields_state %>%
      # Handle cotton special case - for cotton, use average of upland and pima
      group_by(crop, state_name, year) %>%
      summarise(nass_yield_state = mean(nass_yield_state, na.rm = TRUE), .groups = 'drop') %>%
      mutate(state_name = str_to_title(state_name)),
    by = c("crop", "state_name", "program_year" = "year")
  )



# NASS-based yield imputation using 5-year averages
impute_yields_with_nass <- function(data, max_lookback = 5) {
  cat("Starting NASS-based yield imputation using 5-year averages...\n")

  # Sort data for proper lagging
  data <- data %>%
    arrange(fips, crop, crop_type, yield_type, program_year)

  # Create lag variables for multiple years (only past data needed)
  cat("Creating lag variables for", max_lookback, "years...\n")
  for(i in 1:max_lookback) {
    data <- data %>%
      group_by(fips, crop, crop_type, yield_type) %>%
      mutate(
        !!paste0("actual_yield_lag", i) := lag(actual_yield, i),
        !!paste0("nass_state_lag", i) := lag(nass_yield_state, i),
        !!paste0("nass_national_lag", i) := lag(nass_yield_national, i)
      ) %>%
      ungroup()
  }

  # Store original actual_yield for tracking and initialize tracking columns
  data_imputed <- data %>%
    mutate(
      original_actual_yield = actual_yield,
      # Initialize tracking columns
      yield_imputed = FALSE,
      imputation_method = "not_imputed",
      nass_pct_change_applied = NA_real_,
      actual_yield_5yr_avg = NA_real_,
      nass_5yr_avg = NA_real_,
      years_used_in_avg = NA_real_
    )

  cat("Calculating 5-year averages...\n")

  # Calculate 5-year averages for actual yields and NASS data
  data_imputed <- data_imputed %>%
    rowwise() %>%
    mutate(
      # Calculate 5-year average of actual yields (excluding NAs)
      actual_yield_5yr_avg = mean(c(actual_yield_lag1, actual_yield_lag2, actual_yield_lag3,
                                    actual_yield_lag4, actual_yield_lag5), na.rm = TRUE),
      # Count how many years were used in the average
      years_used_in_avg = sum(!is.na(c(actual_yield_lag1, actual_yield_lag2, actual_yield_lag3,
                                       actual_yield_lag4, actual_yield_lag5))),
      # Calculate 5-year average of state NASS yields
      nass_state_5yr_avg = mean(c(nass_state_lag1, nass_state_lag2, nass_state_lag3,
                                  nass_state_lag4, nass_state_lag5), na.rm = TRUE),
      # Calculate 5-year average of national NASS yields
      nass_national_5yr_avg = mean(c(nass_national_lag1, nass_national_lag2, nass_national_lag3,
                                     nass_national_lag4, nass_national_lag5), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Handle cases where no data exists (mean of empty set returns NaN)
    mutate(
      actual_yield_5yr_avg = ifelse(is.nan(actual_yield_5yr_avg), NA, actual_yield_5yr_avg),
      nass_state_5yr_avg = ifelse(is.nan(nass_state_5yr_avg), NA, nass_state_5yr_avg),
      nass_national_5yr_avg = ifelse(is.nan(nass_national_5yr_avg), NA, nass_national_5yr_avg)
    )

  cat("Applying imputation logic...\n")

  # Find records that need imputation and have sufficient data
  missing_indices <- which(
    is.na(data_imputed$actual_yield) &
      !is.na(data_imputed$actual_yield_5yr_avg) &
      data_imputed$years_used_in_avg >= 3  # Require at least 3 years for reliable average
  )

  cat("Processing", length(missing_indices), "missing yield values...\n")

  for(i in missing_indices) {
    row_data <- data_imputed[i, ]
    imputed <- FALSE

    # Try state-level NASS comparison first (preferred method)
    if (!is.na(row_data$nass_yield_state) && !is.na(row_data$nass_state_5yr_avg) && row_data$nass_state_5yr_avg > 0) {
      pct_change <- (row_data$nass_yield_state - row_data$nass_state_5yr_avg) / row_data$nass_state_5yr_avg
      imputed_yield <- row_data$actual_yield_5yr_avg * (1 + pct_change)

      # Quality control: ensure positive yield
      if (imputed_yield > 0) {
        data_imputed$actual_yield[i] <- imputed_yield
        data_imputed$yield_imputed[i] <- TRUE
        data_imputed$imputation_method[i] <- "state_5yr_avg"
        data_imputed$nass_pct_change_applied[i] <- pct_change
        data_imputed$nass_5yr_avg[i] <- row_data$nass_state_5yr_avg
        imputed <- TRUE
      }
    }

    # If state-level failed, try national-level NASS comparison (fallback)
    if (!imputed && !is.na(row_data$nass_yield_national) && !is.na(row_data$nass_national_5yr_avg) && row_data$nass_national_5yr_avg > 0) {
      pct_change <- (row_data$nass_yield_national - row_data$nass_national_5yr_avg) / row_data$nass_national_5yr_avg
      imputed_yield <- row_data$actual_yield_5yr_avg * (1 + pct_change)

      # Quality control: ensure positive yield
      if (imputed_yield > 0) {
        data_imputed$actual_yield[i] <- imputed_yield
        data_imputed$yield_imputed[i] <- TRUE
        data_imputed$imputation_method[i] <- "national_5yr_avg"
        data_imputed$nass_pct_change_applied[i] <- pct_change
        data_imputed$nass_5yr_avg[i] <- row_data$nass_national_5yr_avg
        imputed <- TRUE
      }
    }
  }

  # Clean up temporary lag columns
  lag_cols <- paste0(rep(c("actual_yield_lag", "nass_state_lag", "nass_national_lag"), each = max_lookback), 1:max_lookback)
  temp_cols <- c(lag_cols, "nass_state_5yr_avg", "nass_national_5yr_avg")

  data_imputed <- data_imputed %>%
    select(-all_of(temp_cols))

  # Summary statistics
  n_imputed <- sum(data_imputed$yield_imputed, na.rm = TRUE)
  n_missing_before <- sum(is.na(data$original_actual_yield))
  n_missing_after <- sum(is.na(data_imputed$actual_yield))
  n_insufficient_data <- sum(is.na(data_imputed$actual_yield) &
                               !is.na(data_imputed$original_actual_yield) == FALSE &
                               data_imputed$years_used_in_avg < 3, na.rm = TRUE)

  cat("Imputation complete:\n")
  cat("  Records with missing yields (before):", n_missing_before, "\n")
  cat("  Records successfully imputed:", n_imputed, "\n")
  cat("  Records still missing (after):", n_missing_after, "\n")
  cat("  Records with insufficient historical data (< 3 years):", n_insufficient_data, "\n")

  # Method breakdown
  if (n_imputed > 0) {
    method_summary <- data_imputed %>%
      filter(yield_imputed) %>%
      count(imputation_method, sort = TRUE)
    cat("  Imputation methods used:\n")
    for(i in 1:nrow(method_summary)) {
      cat("    ", method_summary$imputation_method[i], ":", method_summary$n[i], "records\n")
    }
  }

  return(data_imputed %>% select(-original_actual_yield))
}

# Apply NASS-based imputation
data <- impute_yields_with_nass(data, max_lookback = 5)

#' Get RMA (Risk Management Agency) county yield data
#'
#' This function fetches RMA yield data directly from the rfcip package and returns
#' a clean dataframe that can be merged with simulation data.
#' Uses the same methods as process_yields_data.R to fetch and process RMA data.
#' Type codes are fetched using rfcip::get_sob_data() API calls.
#'
#' @param years Vector of years to fetch RMA data for (e.g., 2018:2025)
#' @param practice_filter Optional character vector to filter specific practices
#' @param aggregation_method Method to aggregate multiple practices: "mean", "median", "max" (default: "mean")
#'
#' @return Dataframe with columns: fips, crop, program_year, rma_yield_amount, rma_trended_yield, rma_detrended_yield, rma_practice_count
get_rma_county_yields <- function(years,
                                  practice_filter = NULL,
                                  aggregation_method = "mean") {

  require(dplyr)
  require(arrow)
  require(rfcip)
  require(purrr)

  cat("Fetching RMA yields from rfcip package...\n")

  # Create crop mapping from FSA to RMA commodity names
  rma_crop_mapping <- list(
    "corn" = "CORN",
    "soybeans" = "SOYBEANS",
    "wheat" = "WHEAT",
    "cotton" = "COTTON",
    "rice" = "RICE",
    "sorghum" = "GRAIN SORGHUM",
    "barley" = "BARLEY",
    "peanuts" = "PEANUTS"
  )

  # Validate and clean years parameter
  required_years <- sort(unique(years[!is.na(years)]))

  if (length(required_years) == 0) {
    cat("No valid years provided. Returning empty dataframe.\n")
    return(data.frame())
  }

  cat("Fetching RMA data for years:", paste(required_years, collapse = ", "), "\n")

  # Initialize rma_data as empty data frame in case of errors
  rma_data <- data.frame()

  fetch_success <- tryCatch({
    # Fetch county yield history data (following process_yields_data.R)
    cat("Fetching county yield history...\n")
    rma_data <- rfcip::get_adm_data(dataset = "county_yield_history") %>%
      filter(commodity_year >= min(required_years), commodity_year <= max(required_years))

    cat("Retrieved", nrow(rma_data), "county yield history records\n")

    # Add expected data for 2025 and 2026 if needed
    if (any(required_years >= 2025)) {
      cat("Adding expected yield data for 2025-2026...\n")

      if (2025 %in% required_years) {
        price2025 <- distinct(rfcip::get_adm_data(year = 2025, dataset = "price") %>%
                                select(commodity_year, commodity_code, state_code, county_code, commodity_type_code, practice_code,
                                       expected_index_value, insurance_plan_code) %>%
                                rename(yield_amount = expected_index_value) %>%
                                mutate(type_code = as.numeric(as.character(commodity_type_code))) %>%
                                na.omit())

        rma_data <- bind_rows(rma_data, price2025)
        cat("Added", nrow(price2025), "records for 2025\n")
      }

      if (2026 %in% required_years) {
        price2026 <- distinct(rfcip::get_adm_data(year = 2026, dataset = "price") %>%
                                select(commodity_year, commodity_code, state_code, county_code, commodity_type_code, practice_code,
                                       expected_index_value) %>%
                                rename(yield_amount = expected_index_value,
                                       type_code = commodity_type_code) %>%
                                mutate(state_code = as.numeric(as.character(state_code)),
                                       county_code= as.numeric(as.character(county_code)),
                                       commodity_code = as.numeric(as.character(commodity_code)),
                                       type_code = as.numeric(as.character(type_code)),
                                       practice_code = as.numeric(as.character(practice_code))) %>%
                                na.omit())

        rma_data <- bind_rows(rma_data, price2026)
        cat("Added", nrow(price2026), "records for 2026\n")
      }
    }

    # Replace yields of 0 or 1 with NA (following process_yields_data.R)
    rma_data$yield_amount <- ifelse(rma_data$yield_amount %in% c(0,1), NA, rma_data$yield_amount)

    # Add crop names
    cat("Adding crop names...\n")
    cc <- rfcip::get_crop_codes(year = 2011:max(required_years)) %>%
      mutate(commodity_code = as.numeric(commodity_code),
             commodity_year = as.numeric(commodity_year))

    rma_data <- left_join(rma_data, cc)

    # Create FIPS codes
    cat("Creating FIPS codes...\n")
    rma_data$fips <- rfcip:::clean_fips(
      state = rma_data$state_code,
      county = rma_data$county_code
    )

    # Add state names
    state <- distinct(rfcip::get_adm_data(year = 2024, dataset = "state", show_progress = F) %>%
                        select(state_code, state_name) %>%
                        mutate(
                          state_code = as.numeric(state_code),
                          state_name = as.character(state_name)
                        ))

    rma_data <- left_join(rma_data, state)

    # Add state abbreviations
    rma_data <- left_join(rma_data, data.frame(state.abb, state.name), by = c("state_name" = "state.name")) %>%
      rename(state_abbrv = state.abb)

    # Add county names
    county <- distinct(rfcip::get_adm_data(year = 2024, dataset = "A00440_County") %>%
                         select(state_code, county_code, county_name) %>%
                         mutate(
                           state_code = as.numeric(state_code),
                           county_code = as.numeric(county_code),
                           county_name = as.character(county_name)
                         ))

    rma_data <- left_join(rma_data, county)


    # Add program_year column
    rma_data$program_year <- as.numeric(rma_data$commodity_year)

    # Filter to only required years
    rma_data <- rma_data %>%
      filter(program_year %in% required_years)

    cat("Processed RMA data:", nrow(rma_data), "records\n")

    TRUE  # Success

  }, error = function(e) {
    cat("Error fetching RMA data:", e$message, "\n")
    cat("Returning original data unchanged.\n")
    FALSE  # Failure
  })

  # If fetch failed, return empty dataframe
  if (!fetch_success || nrow(rma_data) == 0) {
    cat("No RMA data retrieved. Returning empty dataframe.\n")
    return(data.frame())
  }

  # Map RMA commodity names to FSA crop names (reverse mapping)
  fsa_crop_mapping <- setNames(names(rma_crop_mapping), unlist(rma_crop_mapping))

  rma_data <- rma_data %>%
    mutate(
      crop = fsa_crop_mapping[commodity_name],
      crop = ifelse(is.na(crop), tolower(commodity_name), crop)
    ) %>%
    filter(!is.na(crop))  # Remove unmapped commodities

  # Apply practice filter if specified
  if (!is.null(practice_filter)) {
    rma_data <- rma_data %>%
      filter(grepl(paste(practice_filter, collapse = "|"), practice_name, ignore.case = TRUE))
    cat("Applied practice filter. Remaining records:", nrow(rma_data), "\n")
  }

  # Aggregate yields by fips, crop, and program_year
  rma_aggregated <- rma_data %>%
    group_by(fips, crop, program_year) %>%
    summarise(
      rma_yield_amount = case_when(
        aggregation_method == "mean" ~ mean(yield_amount, na.rm = TRUE),
        aggregation_method == "median" ~ median(yield_amount, na.rm = TRUE),
        aggregation_method == "max" ~ max(yield_amount, na.rm = TRUE),
        TRUE ~ mean(yield_amount, na.rm = TRUE)
      ),
      rma_trended_yield = case_when(
        aggregation_method == "mean" & "trended_yield_amount" %in% names(rma_data) ~ mean(trended_yield_amount, na.rm = TRUE),
        aggregation_method == "median" & "trended_yield_amount" %in% names(rma_data) ~ median(trended_yield_amount, na.rm = TRUE),
        aggregation_method == "max" & "trended_yield_amount" %in% names(rma_data) ~ max(trended_yield_amount, na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      rma_detrended_yield = case_when(
        aggregation_method == "mean" & "detrended_yield_amount" %in% names(rma_data) ~ mean(detrended_yield_amount, na.rm = TRUE),
        aggregation_method == "median" & "detrended_yield_amount" %in% names(rma_data) ~ median(detrended_yield_amount, na.rm = TRUE),
        aggregation_method == "max" & "detrended_yield_amount" %in% names(rma_data) ~ max(detrended_yield_amount, na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      rma_practice_count = n(),
      .groups = 'drop'
    ) %>%
    # Handle infinite/NaN values
    mutate(
      rma_yield_amount = ifelse(is.infinite(rma_yield_amount) | is.nan(rma_yield_amount), NA, rma_yield_amount),
      rma_trended_yield = ifelse(is.infinite(rma_trended_yield) | is.nan(rma_trended_yield), NA, rma_trended_yield),
      rma_detrended_yield = ifelse(is.infinite(rma_detrended_yield) | is.nan(rma_detrended_yield), NA, rma_detrended_yield)
    ) %>%
    # Select only the columns needed for merging
    select(fips, crop, program_year, rma_yield_amount, rma_trended_yield, rma_detrended_yield, rma_practice_count)

  cat("Returning RMA data:", nrow(rma_aggregated), "unique fips/crop/year combinations\n")

  return(rma_aggregated)
}

# Get RMA yields data
rma_yields <- get_rma_county_yields(years = 2011:max(data$program_year, na.rm = T))

# Merge RMA yields with simulation data
data <- data %>%
  left_join(rma_yields %>% mutate(fips = as.numeric(fips)), by = c("fips", "crop", "program_year"))

# if actual yield is still missing, fill in with rma_yield_amount
data <- data %>%
  mutate(
    actual_yield = ifelse(
      is.na(actual_yield) & !is.na(rma_yield_amount),
      rma_yield_amount,
      actual_yield
    )
  )


# if actual yield is still missing, fill in with plc_yield
data <- data %>%
  mutate(
    actual_yield = ifelse(
      is.na(actual_yield) & !is.na(plc_yield),
      plc_yield,
      actual_yield
    )
  )


# linearly interpolate any still missing actual_yields
data <- data %>%
  group_by(fips, crop, crop_type) %>%
  arrange(fips, crop, crop_type, program_year) %>%
  mutate(
    actual_yield = zoo::na.approx(actual_yield, na.rm = FALSE, rule = 2)
  ) %>%
  ungroup()


# fill in national_price with current_mya_price where missing
data$national_price[is.na(data$national_price)] <-
  data$current_mya_price[is.na(data$national_price)]

# fill in missing actual revenues using actual yield * national price
data <- data %>%
  mutate(
    actual_revenue = ifelse(
      is.na(actual_revenue) & !is.na(actual_yield) & !is.na(national_price),
      actual_yield * national_price,
      actual_revenue
    )
  )





# Export data as RDA file for package data
fsaArcPlcData <- data
usethis::use_data(fsaArcPlcData, overwrite = TRUE)

# # check national levels
# national_validation <- data.frame(year = 2019:2025, calc_plc = NA, act_plc = NA, plc_diff = NA, calc_arc = NA, act_arc = NA, arc_diff = NA)
#
# for(y in national_validation$year){
#
#
#   # plc
#   try({
#     national_validation$calc_plc[which(national_validation$year == y)] = sum(data$plc_payment[which(data$program_year == y)]*data[which(data$program_year == y),paste0("enrolled_base_PLC")]*(1-.068), na.rm = T)/1000000
#
#     act = rfsa::get_fsa_payments(year = y, program = "PLC", year_type = "program")
#     if(nrow(act) == 0){
#       amount = 0
#     } else {
#       amount = act$payment_amount/1000000
#     }
#     national_validation$act_plc[which(national_validation$year == y)]  = amount
#     national_validation$plc_diff[which(national_validation$year == y)] <- paste0(round((national_validation$calc_plc[which(national_validation$year == y)] - national_validation$act_plc[which(national_validation$year == y)])/national_validation$act_plc[which(national_validation$year == y)],2)*100,"%")
#   })
#
#   # arc
#   try({
#     national_validation$calc_arc[which(national_validation$year == y)] = sum(data$arc_payment[which(data$program_year == y)]*data[which(data$program_year == y),paste0("enrolled_base_ARCCO")]*(1-.068), na.rm = T)/1000000
#
#     act = rfsa::get_fsa_payments(year = y, program = "ARC-CO", year_type = "program")
#     if(nrow(act) == 0){
#       amount = 0
#     } else {
#       amount = act$payment_amount/1000000
#     }
#     national_validation$act_arc[which(national_validation$year == y)]  = amount
#     national_validation$arc_diff[which(national_validation$year == y)] <- paste0(round((national_validation$calc_arc[which(national_validation$year == y)] - national_validation$act_arc[which(national_validation$year == y)])/national_validation$act_arc[which(national_validation$year == y)],2)*100,"%")
#   })
#
#
# }
# national_validation
#
# national_validation[national_validation == "NaN%"] <- "0%"
#
#



