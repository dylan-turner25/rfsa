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
library(stringr)
library(dplyr)
library(tidyr)

data <- fsaArcCoBenchmarks %>%
  mutate(marketing_year = paste0(program_year, "-", program_year + 1))


# get a data frame of unique fips observations
unique_fips <- data %>%
  select(fips, state_name) %>%
  distinct()

unique_crops <- data %>%
  select(crop, crop_type) %>%
  distinct()

unique_years <- data %>%
  select(program_year) %>%
  distinct()

# create a data frame that is every combination of unique fips, crop, yield_type, and year
data <- tidyr::crossing(unique_fips, unique_crops, unique_years)

# marketing year goes on the skeleton itself so the price joins below match
# county-crops that FSA did not benchmark that year (PLC needs no benchmark)
data <- data %>%
  mutate(marketing_year = paste0(program_year, "-", program_year + 1))

# Carry the most recent observed year of a source table forward to any program
# years present in the data skeleton but missing from the source. Used for
# slow-moving farm attributes (PLC yields, base acres) and for pre-election
# future-year assumptions (enrollment elections, irrigated planting mix).
# Self-superseding: once a real FSA release is added to input_data/ and the
# upstream script is rerun, the source table contains that year and nothing
# is carried forward for it.
extend_to_skeleton_years <- function(df, year_col = "program_year",
                                     skeleton_years = unique_years$program_year) {
  max_yr <- max(df[[year_col]], na.rm = TRUE)
  future <- sort(unique(skeleton_years[skeleton_years > max_yr]))
  if (length(future) == 0) return(df)
  carried <- lapply(future, function(y) {
    out <- df[df[[year_col]] == max_yr, , drop = FALSE]
    out[[year_col]] <- y
    out
  })
  dplyr::bind_rows(df, carried)
}

# start with arc co benchmarks
benchmarks <- fsaArcCoBenchmarks %>%
  mutate(marketing_year = paste0(program_year, "-", program_year + 1))

data <- left_join(data, benchmarks)


# merge in plc yield
# PLC yields are fixed farm attributes; carry the latest published year forward
# to future program years until FSA releases updated yield files
# one row per key: a county listed under two names (e.g. Henrico /
# Henrico-Richmond City, VA in 2024-2025) gets the average of its PLC yields
plc_yields <- fsaPlcYields %>%
  group_by(fips, crop, crop_type, program_year) %>%
  summarize(plc_yield = mean(plc_yield, na.rm = TRUE), .groups = "drop") %>%
  mutate(plc_yield = ifelse(is.nan(plc_yield), NA, plc_yield)) %>%
  extend_to_skeleton_years()
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
    select(statutory_reference_price, crop, crop_type,program_year)
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
      # OBBBA changes the olympic average multiplier from 85% to 88%
      # beginning with the 2026 crop year (ERP cap remains 115% of SRP)
      rfsa:::calc_effective_reference_price(
        mya_prices = mya_prices,
        srp = data$statutory_reference_price[i],
        oa_pct = ifelse(data$program_year[i] >= 2026, 0.88, 0.85)
      )
    },
    error = function(e) {
      # Return NA on any error
      return(NA)
    }
  )
}))

# published ERPs are rounded, so compare with a 0.5% relative tolerance
# rather than exact equality
data$erp_calc_check <- as.numeric(
  abs(data$erp_calc / data$effective_reference_price - 1) < 0.005
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


# for missing base acres, fill in with the most recent base acres available for that county/crop/crop_type
# split into pre-2018 and post-2018 to use different fill directions (i.e. fromLast = TRUE for post-2018)
base2014 <- base %>% filter(program_year == 2014)
base2015 <- base %>% filter(program_year == 2015)
base2016 <- base %>% filter(program_year == 2015) %>% mutate(program_year = 2016)
base2017 <- base %>% filter(program_year == 2015) %>% mutate(program_year = 2017)
base2018 <- base %>% filter(program_year == 2015) %>% mutate(program_year = 2018)
base2019 <- base %>% filter(program_year == 2021) %>% mutate(program_year = 2019)
base2020 <- base %>% filter(program_year == 2021) %>% mutate(program_year = 2020)
base2021 <- base %>% filter(program_year == 2021)
base2022 <- base %>% filter(program_year == 2022)
base2023 <- base %>% filter(program_year == 2023)

# bind together all base data frames
base <- bind_rows(base2014, base2015, base2016, base2017, base2018, base2019,
                  base2020, base2021, base2022, base2023)

# carry the most recent base acre release forward to any later skeleton years
# (currently 2023 -> 2024+, until FSA publishes updated county base acre files)
base <- extend_to_skeleton_years(base)

# merge in with data
data <- left_join(data, base)

# merge in enrolled base acres (current year)
# For program years where elections have not yet occurred, carry forward the
# most recent year's ARC/PLC elections as the pre-election assumption
# (i.e. producers are assumed to keep their existing elections)
# distinct() drops repeat records of one county listed under two names
# (e.g. Henrico / Henrico-Richmond City, VA in 2023-2024)
enrolled_base <- fsaEnrolledCountyBaseAcres %>%
  select(fips, program_year, crop, crop_type, contains("enrolled")) %>%
  distinct() %>%
  extend_to_skeleton_years()

data <- left_join(data, enrolled_base)


# merge in planted acres (assume all cotton acres are seed cotton)
planted_acres <- fsaCropAcreageCC %>%
  mutate(
    crop_type = gsub("upland|extra long staple", "seed", crop_type)
  ) %>%
  group_by(crop_yr, crop, crop_type, fips, irrigation_practice) %>%
  summarize(
    planted_and_failed_acres = sum(planted_and_failed_acres, na.rm = TRUE),
    prevented_acres = sum(prevented_acres, na.rm = TRUE),
    planted_acres = sum(planted_acres, na.rm = TRUE),
    failed_acres = sum(failed_acres, na.rm = TRUE),
    .groups = "drop"
  )

# Carry the most recent planting mix forward for irrigation SHARE calculations
# only. The planted/prevented/failed acre OUTCOME columns are intentionally NOT
# carried forward (they are realized outcomes, not farm attributes, and feed no
# payment calculation); only the irrigated vs non-irrigated shares carry into
# future years so the base acre irrigation adjustment below does not NA-out
# future-year rows.
planted_acres_ext <- extend_to_skeleton_years(planted_acres, year_col = "crop_yr")

# Create detailed irrigation summary by year, crop, crop_type, and county
irrigation_summary <- planted_acres_ext %>%
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

irrigation_summary_crop <- planted_acres_ext %>%
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

# adjust base acres by irrigation status
# Base and enrollment are county x crop figures, joined in full onto every
# yield_type row of a county-crop-year, so only one representation may carry
# them. FSA's benchmark files sometimes list All alongside Irrigated and
# Nonirrigated rows, with only one of the two benchmarked (2020-2024 has
# blank split rows next to a benchmarked All row). Base goes to whichever rows
# FSA benchmarked that year: the split rows if any has a benchmark, otherwise
# the All row, otherwise (no All row) the split rows. Split rows then share the
# base by planted share, rescaled over the split rows present so a county with
# only one split row keeps all of its base. Rows with no yield_type keep theirs.
data <- data %>%
  group_by(fips, crop, crop_type, program_year) %>%
  mutate(
    .is_split = yield_type %in% c("Irrigated", "Nonirrigated"),
    .split_bm = any(.is_split & !is.na(oa_bench_mark_yield)),
    .has_all = any(yield_type %in% "All"),
    .carries = case_when(
      is.na(yield_type) ~ TRUE,
      .is_split ~ .split_bm | !.has_all,
      TRUE ~ !.split_bm
    ),
    .share = case_when(
      yield_type == "Irrigated" ~ planted_irrigated_share,
      yield_type == "Nonirrigated" ~ planted_non_irrigated_share,
      TRUE ~ 1
    ),
    .split_total = sum(.share[.is_split & .carries]),
    .n_split = sum(.is_split & .carries),
    # even split if planted shares are missing or zero
    .share = case_when(
      !.carries ~ 0,
      !.is_split ~ 1,
      is.na(.split_total) | .split_total == 0 ~ 1 / .n_split,
      TRUE ~ .share / .split_total
    ),
    .src_enrolled = first(enrolled_base_ARCCO + enrolled_base_PLC),
    base_acres = base_acres * .share,
    enrolled_base_ARCCO = enrolled_base_ARCCO * .share,
    enrolled_base_PLC = enrolled_base_PLC * .share
  ) %>%
  ungroup()

# each county-crop-year must carry its enrolled base exactly once
base_check <- data %>%
  filter(!is.na(yield_type)) %>%
  group_by(fips, crop, crop_type, program_year) %>%
  summarize(
    src = first(.src_enrolled),
    panel = sum(enrolled_base_ARCCO + enrolled_base_PLC),
    .groups = "drop"
  ) %>%
  filter(!is.na(src), abs(panel - src) > 0.01)
if (nrow(base_check) > 0) {
  stop(nrow(base_check), " county-crop-years carry enrolled base more or ",
       "less than once after the irrigation split")
}

# .carries and .n_split are kept for the planted acre allocation below
data <- data %>%
  select(-.is_split, -.split_bm, -.has_all, -.share, -.split_total,
         -.src_enrolled)

# every row with PLC enrolled base needs an MYA price, benchmarked or not
plc_check <- data %>%
  filter(coalesce(enrolled_base_PLC, 0) > 0)
if (any(is.na(plc_check$current_mya_price))) {
  stop(sum(is.na(plc_check$current_mya_price)), " rows with PLC enrolled ",
       "base have no MYA price")
}
# missing PLC yields are left to the yield fallbacks; report them here
message(sum(is.na(plc_check$plc_yield)), " rows with PLC enrolled base ",
        "have no PLC yield")



# Planted acres are county x crop outcomes, so like base they sit once per
# county-crop-year on the rows that carry base: All rows and rows with no
# benchmark take the county total, split rows their own practice, and a lone
# split row the total. Future years get none (outcomes are not carried forward).
acre_cols <- c("planted_and_failed_acres", "prevented_acres", "planted_acres",
               "failed_acres")

planted_acres_harmonized <- planted_acres %>%
  filter(irrigation_practice %in% c("I", "N")) %>%
  mutate(.acre_type = ifelse(irrigation_practice == "I", "Irrigated",
                             "Nonirrigated")) %>%
  select(crop_yr, crop, crop_type, fips, .acre_type, all_of(acre_cols)) %>%
  bind_rows(
    planted_acres %>%
      group_by(crop_yr, crop, crop_type, fips) %>%
      summarize(across(all_of(acre_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(.acre_type = "All")
  )

data <- data %>%
  mutate(.acre_type = case_when(
    !.carries ~ NA_character_,
    yield_type %in% c("Irrigated", "Nonirrigated") & .n_split > 1 ~ yield_type,
    TRUE ~ "All"
  )) %>%
  left_join(
    planted_acres_harmonized,
    by = c("program_year" = "crop_yr", "crop", "crop_type", "fips",
           ".acre_type")
  )

# if no planted acres are reported, set planted acres to zero
data <- data %>%
  mutate(across(all_of(acre_cols), ~ coalesce(.x, 0)))

# each county-crop-year must carry its planted acres exactly once. Acres with
# no irrigation practice reach only the All total, so split rows may fall
# short of the source by those acres and no more.
acre_src <- planted_acres %>%
  group_by(program_year = crop_yr, crop, crop_type, fips) %>%
  summarize(
    src = sum(planted_acres, na.rm = TRUE),
    src_unknown = sum(planted_acres[!irrigation_practice %in% c("I", "N")],
                      na.rm = TRUE),
    .groups = "drop"
  )
acre_check <- data %>%
  group_by(program_year, crop, crop_type, fips) %>%
  summarize(panel = sum(planted_acres), .groups = "drop") %>%
  inner_join(acre_src, by = c("program_year", "crop", "crop_type", "fips"))
acre_bad <- acre_check %>%
  filter(panel > src + 0.01 | panel < src - src_unknown - 0.01)
if (nrow(acre_bad) > 0) {
  stop(nrow(acre_bad), " county-crop-years carry planted acres more or ",
       "less than once")
}
acre_off_panel <- acre_src %>%
  semi_join(data, by = c("crop", "crop_type")) %>%
  anti_join(acre_check, by = c("program_year", "crop", "crop_type", "fips"))
message(round(sum(acre_check$src - acre_check$panel)), " planted acres with ",
        "no irrigation practice left off split rows; ",
        round(sum(acre_off_panel$src)), " planted acres of program crops on ",
        "county-crops outside the panel")

data <- data %>%
  select(-.carries, -.n_split, -.acre_type)


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
  oats = "OATS",
  sunflower = "SUNFLOWER",
  canola = "CANOLA",
  flaxseed = "FLAXSEED",
  "dry peas" = "PEAS",
  lentils = "LENTILS",
  chickpeas = "CHICKPEAS",
  safflower = "SAFFLOWER"
)

# NASS series to keep where a commodity has several (regex on short_desc).
# Applied before picking the latest load so another series' later release
# cannot crowd out the one we want. Green peas are in CWT; sunflower and
# chickpeas also publish by-class series next to the all-class total.
nass_series_filter <- list(
  cotton = "UPLAND",
  "dry peas" = "^PEAS, DRY EDIBLE - ",
  sunflower = "^SUNFLOWER - ",
  chickpeas = "^CHICKPEAS - "
)

#' Get National NASS Yields
#'
#' Collects national-level NASS yield data for all commodities
#'
#' @param years vector of years to collect data for
#' @param commodity_mapping list mapping crop names to NASS commodity descriptions
#' @return dataframe with national NASS yields by commodity and year
get_nass_yields_national <- function(years = 2014:2026, commodity_mapping = nass_commodity_mapping) {
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
          filter(source_desc == "SURVEY")

        if (!is.null(nass_series_filter[[crop_name]])) {
          yields <- yields %>%
            filter(grepl(nass_series_filter[[crop_name]], short_desc))
        }

        yields <- yields %>%
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
get_nass_yields_state <- function(years = 2014:2026, commodity_mapping = nass_commodity_mapping) {
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
          filter(source_desc == "SURVEY")

        if (!is.null(nass_series_filter[[crop_name]])) {
          yields <- yields %>%
            filter(grepl(nass_series_filter[[crop_name]], short_desc))
        }

        yields <- yields %>%
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
# five years before the first program year, for the NASS ratios
nass_years <- (min(data$program_year, na.rm = TRUE) - 5):max(data$program_year, na.rm = TRUE)
nass_yields_national <- get_nass_yields_national(years = nass_years) %>%
  mutate(year = as.integer(year)) %>%
  filter(!grepl("silage", tolower(short_desc))) %>%
  group_by(crop, year) %>%
  summarise(nass_yield_national = mean(nass_yield_national, na.rm = TRUE), .groups = 'drop')

nass_yields_state <- get_nass_yields_state(years = nass_years) %>%
  mutate(year = as.integer(year)) %>%
  filter(!grepl("silage", tolower(short_desc))) %>%
  group_by(crop, state_name, year) %>%
  summarise(nass_yield_state = mean(nass_yield_state, na.rm = TRUE), .groups = 'drop')

# NASS ratio: a year's yield over its average in the five prior years, with
# at least three of those years reported. Built from the NASS tables, not the
# panel, so a row without earlier panel rows (e.g. a split row FSA first
# benchmarks this year) still gets one.
add_nass_ratio <- function(nass, value_col, keys) {
  nass %>%
    rename(.y = all_of(value_col)) %>%
    group_by(across(all_of(keys))) %>%
    mutate(
      .n_prior = sapply(year, function(t) sum(year %in% (t - 5):(t - 1))),
      .avg = sapply(year, function(t) mean(.y[year %in% (t - 5):(t - 1)])),
      .avg = ifelse(.n_prior >= 3, .avg, NA_real_)
    ) %>%
    ungroup() %>%
    mutate(.ratio = .y / .avg) %>%
    select(-.n_prior) %>%
    rename(
      !!value_col := .y,
      !!paste0(value_col, "_5yr_avg") := .avg,
      !!paste0(value_col, "_ratio") := .ratio
    )
}

nass_national <- nass_yields_national %>%
  group_by(crop, year) %>%
  summarise(nass_yield_national = mean(nass_yield_national, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(nass_yield_national)) %>%
  add_nass_ratio("nass_yield_national", "crop")

nass_state <- nass_yields_state %>%
  mutate(state_name = str_to_title(state_name)) %>%
  group_by(crop, state_name, year) %>%
  summarise(nass_yield_state = mean(nass_yield_state, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(nass_yield_state)) %>%
  add_nass_ratio("nass_yield_state", c("crop", "state_name"))

data <- data %>%
  left_join(nass_national, by = c("crop", "program_year" = "year")) %>%
  left_join(nass_state, by = c("crop", "state_name", "program_year" = "year"))

# Yield cascade. Where FSA published no actual yield, take the first of:
#   1 state_5yr_avg       own 5-yr average of FSA actuals x NASS state ratio
#   2 national_5yr_avg    own 5-yr average x NASS national ratio
#   3 benchmark_state     FSA benchmark yield x NASS state ratio
#   4 benchmark_national  FSA benchmark yield x NASS national ratio
#   5 rma_yield           RMA realized county yield, else
#     rma_expected        RMA expected county yield (below)
#   6 benchmark_yield     FSA benchmark yield, unscaled (below)
# Steps 1-2 need FSA actuals in at least three of the five prior program
# years, matched by year rather than row position. Rows left without a yield
# have no benchmark, so they cannot pay ARC-CO.
data <- data %>%
  arrange(fips, crop, crop_type, yield_type, program_year) %>%
  group_by(fips, crop, crop_type, yield_type) %>%
  mutate(
    .lag1 = actual_yield[match(program_year - 1, program_year)],
    .lag2 = actual_yield[match(program_year - 2, program_year)],
    .lag3 = actual_yield[match(program_year - 3, program_year)],
    .lag4 = actual_yield[match(program_year - 4, program_year)],
    .lag5 = actual_yield[match(program_year - 5, program_year)]
  ) %>%
  ungroup() %>%
  mutate(
    years_used_in_avg = rowSums(!is.na(across(.lag1:.lag5))),
    actual_yield_5yr_avg = ifelse(years_used_in_avg >= 3,
                                  rowMeans(across(.lag1:.lag5), na.rm = TRUE),
                                  NA_real_),
    imputation_method = case_when(
      !is.na(actual_yield) ~ "not_imputed",
      !is.na(actual_yield_5yr_avg) & !is.na(nass_yield_state_ratio) ~ "state_5yr_avg",
      !is.na(actual_yield_5yr_avg) & !is.na(nass_yield_national_ratio) ~ "national_5yr_avg",
      !is.na(oa_bench_mark_yield) & !is.na(nass_yield_state_ratio) ~ "benchmark_state",
      !is.na(oa_bench_mark_yield) & !is.na(nass_yield_national_ratio) ~ "benchmark_national",
      TRUE ~ "missing"
    ),
    nass_ratio = case_when(
      imputation_method %in% c("state_5yr_avg", "benchmark_state") ~ nass_yield_state_ratio,
      imputation_method %in% c("national_5yr_avg", "benchmark_national") ~ nass_yield_national_ratio
    ),
    nass_5yr_avg = case_when(
      imputation_method %in% c("state_5yr_avg", "benchmark_state") ~ nass_yield_state_5yr_avg,
      imputation_method %in% c("national_5yr_avg", "benchmark_national") ~ nass_yield_national_5yr_avg
    ),
    nass_pct_change_applied = nass_ratio - 1,
    actual_yield = case_when(
      imputation_method %in% c("state_5yr_avg", "national_5yr_avg") ~ actual_yield_5yr_avg * nass_ratio,
      imputation_method %in% c("benchmark_state", "benchmark_national") ~ oa_bench_mark_yield * nass_ratio,
      TRUE ~ actual_yield
    )
  ) %>%
  select(-.lag1, -.lag2, -.lag3, -.lag4, -.lag5, -nass_ratio,
         -nass_yield_state_5yr_avg, -nass_yield_state_ratio,
         -nass_yield_national_5yr_avg, -nass_yield_national_ratio)

# RMA county yields (cascade step 5) ===========================================
# Step 5 fills rows that have no NASS ratio: minor oilseeds NASS does not
# survey, or every crop when a projection is made before NASS publishes the
# year. Two RMA sources, both at county x commodity x type x practice x year:
#   realized: county yields from RMA's county yield history
#   expected: the expected county yield RMA sets for area plans each crop year
#             (expected_index_value in the ADM price file), published before
#             planting
# A realized yield is used where one exists, else the expected yield.
# Practices: 002 maps to Irrigated rows and 003 to Nonirrigated rows; where a
# county has no 003, the dryland practices 004-006 (continuous cropping,
# summerfallow, water fallow) stand in, and 997 (no practice specified, used
# for flax) fills either side. Organic practices (700s) repeat these values and
# are ignored. All rows and rows with no yield_type take the irrigated and
# nonirrigated yields blended by planted share.
# Cotton: RMA reports lint; FSA covers seed cotton, 2.4 lb per lb of lint
# (1 lb lint + 1.4 lb cottonseed). Sesame is left out: RMA's county yields
# run about 35% below FSA's benchmarks (median 0.65), and crambe has no RMA
# coverage.

rma_crop_map <- bind_rows(
  tibble(
    commodity_code = c(41, 81, 11, 21, 51, 91, 16, 75, 15, 15, 78, 31, 49, 69),
    crop = c("corn", "soybeans", "wheat", "cotton", "grain sorghum", "barley",
             "oats", "peanuts", "canola", "rapeseed", "sunflower", "flaxseed",
             "safflower", "mustard"),
    crop_type = c(NA, NA, NA, "seed", rep(NA, 10)),
    type_code = NA_real_
  ),
  # rice types: 451 short, 452 medium, 453 long grain
  tibble(commodity_code = 18, type_code = c(453, 451, 452, 451, 452), crop = "rice",
         crop_type = c("long grain", "short/medium grain", "short/medium grain",
                       "temperate japonica", "temperate japonica")),
  # dry peas types: peas, lentils, chickpeas (contract seed peas and fava
  # beans are left out)
  tibble(commodity_code = 67,
         type_code = c(89, 95, 97, 189, 197, 99, 199, 90, 290, 91, 92),
         crop = c(rep("dry peas", 5), "lentils", "lentils", rep("chickpeas", 4)),
         crop_type = c(rep(NA, 7), "large", "large", "small", "small"))
) %>%
  mutate(lint_factor = ifelse(crop == "cotton", 2.4, 1))

get_rma_county_yields <- function(years) {
  codes <- function(df) {
    tibble::as_tibble(df) %>% mutate(across(any_of(c("state_code", "county_code", "commodity_code", "commodity_type_code",
                                  "type_code", "practice_code", "commodity_year")),
                         ~ as.numeric(as.character(.x))))
  }

  cat("Fetching RMA county yield history...\n")
  realized <- rfcip::get_adm_data(dataset = "county_yield_history") %>%
    codes() %>%
    filter(commodity_year %in% years) %>%
    transmute(year = commodity_year, state_code, county_code, commodity_code,
              type_code, practice_code, yield = yield_amount, source = "realized")
  if (nrow(realized) == 0) stop("RMA county yield history returned no rows for ",
                                min(years), "-", max(years))

  expected <- purrr::map_dfr(years, function(yr) {
    price <- rfcip::get_adm_data(year = yr, dataset = "price", show_progress = FALSE)
    if (!"expected_index_value" %in% names(price)) {
      cat("  ", yr, ": no expected county yields in the RMA price file\n")
      return(NULL)
    }
    price %>%
      codes() %>%
      transmute(year = yr, state_code, county_code, commodity_code,
                type_code = commodity_type_code, practice_code,
                yield = as.numeric(expected_index_value), source = "expected") %>%
      distinct()
  })
  if (nrow(expected) == 0) stop("RMA price files returned no expected county yields")

  rma <- bind_rows(realized, expected) %>%
    filter(!is.na(yield), !yield %in% c(0, 1)) %>%
    mutate(
      fips = state_code * 1000 + county_code,
      class = case_when(practice_code == 2 ~ "irr",
                        practice_code == 3 ~ "non",
                        practice_code %in% 4:6 ~ "non_alt",
                        practice_code == 997 ~ "nips")
    ) %>%
    filter(!is.na(class))

  # crops mapped by commodity alone, then rice and dry peas by type
  by_crop <- rma %>%
    inner_join(filter(rma_crop_map, is.na(type_code)) %>% select(-type_code),
               by = "commodity_code", relationship = "many-to-many")
  by_type <- rma %>%
    inner_join(filter(rma_crop_map, !is.na(type_code)),
               by = c("commodity_code", "type_code"), relationship = "many-to-many")

  out <- bind_rows(by_crop, by_type) %>%
    group_by(fips, crop, crop_type, year, source, class) %>%
    summarise(yield = mean(yield * lint_factor), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = c(class, source), values_from = yield)
  for (col in c("irr_realized", "non_realized", "non_alt_realized", "nips_realized",
                "irr_expected", "non_expected", "non_alt_expected", "nips_expected")) {
    if (!col %in% names(out)) out[[col]] <- NA_real_
  }
  out <- out %>%
    mutate(irr_realized = coalesce(irr_realized, nips_realized),
           non_realized = coalesce(non_realized, non_alt_realized, nips_realized),
           irr_expected = coalesce(irr_expected, nips_expected),
           non_expected = coalesce(non_expected, non_alt_expected, nips_expected)) %>%
    select(fips, crop, crop_type, program_year = year,
           irr_realized, non_realized, irr_expected, non_expected)

  cat("RMA county yields:", nrow(out), "county-crop-years\n")
  print(out %>% group_by(program_year) %>%
          summarise(realized = sum(!is.na(irr_realized) | !is.na(non_realized)),
                    expected = sum(!is.na(irr_expected) | !is.na(non_expected))),
        n = Inf)
  out
}

# yield for a row's yield_type; All and missing yield_type blend the two
# practices by planted share (equal weights where shares are missing)
rma_row_yield <- function(yield_type, irr, non, s_irr, s_non) {
  s_irr <- coalesce(s_irr, 0.5)
  s_non <- coalesce(s_non, 0.5)
  blend <- ifelse(s_irr + s_non > 0,
                  (s_irr * irr + s_non * non) / (s_irr + s_non),
                  (irr + non) / 2)
  case_when(
    yield_type %in% "Irrigated" ~ irr,
    yield_type %in% "Nonirrigated" ~ non,
    !is.na(irr) & !is.na(non) ~ blend,
    TRUE ~ coalesce(non, irr)
  )
}

rma_yields <- get_rma_county_yields(years = 2014:max(data$program_year, na.rm = TRUE))

data <- data %>%
  left_join(rma_yields, by = c("fips", "crop", "crop_type", "program_year")) %>%
  mutate(
    .rma_realized = rma_row_yield(yield_type, irr_realized, non_realized,
                                  planted_irrigated_share, planted_non_irrigated_share),
    .rma_expected = rma_row_yield(yield_type, irr_expected, non_expected,
                                  planted_irrigated_share, planted_non_irrigated_share),
    rma_yield_amount = coalesce(.rma_realized, .rma_expected),
    rma_yield_source = case_when(!is.na(.rma_realized) ~ "realized",
                                 !is.na(.rma_expected) ~ "expected")
  ) %>%
  select(-irr_realized, -non_realized, -irr_expected, -non_expected,
         -.rma_realized, -.rma_expected)

# RMA and FSA yields must be in the same units. A unit error (cotton lint
# against seed cotton, pounds against bushels) moves the median ratio to the
# benchmark far outside this band.
rma_check <- data %>%
  filter(!is.na(rma_yield_amount), coalesce(oa_bench_mark_yield, 0) > 0) %>%
  group_by(crop) %>%
  summarise(n = n(), ratio = median(rma_yield_amount / oa_bench_mark_yield),
            .groups = "drop")
print(rma_check, n = Inf)
rma_bad <- filter(rma_check, n >= 20, ratio < 0.7 | ratio > 1.4)
if (nrow(rma_bad) > 0) {
  stop("RMA yields out of line with FSA benchmarks (median ratio) for: ",
       paste0(rma_bad$crop, " ", round(rma_bad$ratio, 2), collapse = ", "))
}

# cascade step 5: RMA county yield, realized where published, else expected
data <- data %>%
  mutate(
    .use = imputation_method == "missing" & !is.na(rma_yield_amount),
    actual_yield = ifelse(.use, rma_yield_amount, actual_yield),
    imputation_method = case_when(
      !.use ~ imputation_method,
      rma_yield_source == "realized" ~ "rma_yield",
      TRUE ~ "rma_expected"
    )
  ) %>%
  select(-.use)

# cascade step 6: FSA benchmark yield, unscaled (the county's expected yield)
data <- data %>%
  mutate(
    .use = imputation_method == "missing" & !is.na(oa_bench_mark_yield),
    actual_yield = ifelse(.use, oa_bench_mark_yield, actual_yield),
    imputation_method = ifelse(.use, "benchmark_yield", imputation_method)
  ) %>%
  select(-.use) %>%
  mutate(yield_imputed = !imputation_method %in% c("not_imputed", "missing"))

print(count(data, imputation_method))

# every row that can pay ARC-CO needs an actual yield
arc_check <- data %>%
  filter(coalesce(enrolled_base_ARCCO, 0) > 0, !is.na(oa_bench_mark_yield),
         is.na(actual_yield))
if (nrow(arc_check) > 0) {
  stop(nrow(arc_check), " rows with ARC-CO base and a benchmark have no ",
       "actual yield")
}


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



