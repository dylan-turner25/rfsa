data("fsaArcCoBenchmarks")
data("fsaMyaPrice")
data("fsaPlcYields")
data("fsaEffectiveRefPrices")
data("fsaArcCoPrice")
data("fsaCountyBaseAcres")
data("fsaEnrolledCountyBaseAcres")
data("fsaCountyBaseAcres")
data("fsaCropAcreageCC")

data <- fsaArcCoBenchmarks %>%
  mutate(marketing_year = paste0(program_year, "-", program_year + 1))

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

# add missing oa_benchark_prices_lag using the historical values of the oa_benchark_price column

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

# merge in total base acres (current year)
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

planted_acres %>%
  filter(crop == "peanuts") %>%
  group_by(crop_yr) %>%
  summarize(
    planted_acres = sum(planted_acres, na.rm = TRUE)
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



data %>%
  filter(crop == "peanuts") %>%
  group_by(program_year) %>%
  summarize(
    planted_acres = sum(planted_acres, na.rm = TRUE)
  )

# do some data validation against actual planted acres
sum(
  data$planted_acres[data$crop == "corn" & data$program_year == 2023],
  na.rm = T
)
sum(
  data$planted_acres[data$crop == "rice" & data$program_year == 2023],
  na.rm = T
)
sum(
  data$planted_acres[data$crop == "cotton" & data$program_year == 2021],
  na.rm = T
)


# Base acre data, including total base acres and enrolled base acres are adjusted in accordance
# with the share of planted acres that are irrigated vs non-irrigated in each county for each crop.
# In cases where no planted acres data are avaliable (i.e. planted acres are zero for the county and crop),
# the national level share of irrigated vs non-irrigated acres for that crop is used to adjust base acres proportionally.

# Note: at this point, base acres are duplicated for irrigation status, i.e. if irrigated and non-irrigated yields are reported,
# base acres are duplicated since we don't have irrigation status in the base acres data. RaFF addressed this by assuming all acres were non-irrigated
# and using the census of agriculture to estimate the share of irrigated acres by county and applying base acres proportionally. This could be done in the future,
# in which case the code to do so would be placed here. For now, we are assuming non-irrigated and filtering out irrigated observations.
#data <- data %>%
#  filter(yield_type != "Irrigated")

# get a count of observations by program_year
table(data$program_year)

# get a count of non missing actual yields by program_year
table(data$program_year[!is.na(data$actual_yield)])

# if no planted acres are reported, set planted acres to zero
data$planted_acres[is.na(data$planted_acres)] <- 0

# filter years
#data <- data %>% filter(program_year >= 2022 & program_year <= 2023)
#data <- data %>% filter(program_year >= 2019 & program_year <= 2025)

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



# calculate arc and plc payments per base acre under the policy environment of the 2018 Farm Bill -----------------------

# calculate plc payment per base acres
tic()
data$plc_payment <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch(
    {
      # Extract historic MYA prices for this row
      historical_prices <- c(
        data$annual_benchmark_price_lag1[i],
        data$annual_benchmark_price_lag2[i],
        data$annual_benchmark_price_lag3[i],
        data$annual_benchmark_price_lag4[i],
        data$annual_benchmark_price_lag5[i]
      )

      # Calculate PLC payment
      result <- rfsa::calc_plc_payment(
        crop = data$crop[i],
        crop_type = data$crop_type[i],
        program_year = data$program_year[i],
        base_acres = 1,
        mya_price = data$current_mya_price[i],
        historic_mya_prices = historical_prices,
        srp = data$statutory_reference_price[i],
        erp = NULL,
        always_use_erp = FALSE,
        nmlr = data$current_national_loan_rate[i],
        plc_yield = data$plc_yield[i],
        cov_lvl = 0.85,
        fips = data$fips[i],
        quiet = TRUE
      )

      return(result)
    },
    error = function(e) {
      # Return NA on any error
      return(NA)
    }
  )
}))
toc()

# calculate arc payment per base acre

# Prepare vectorized inputs for optimized function
historical_prices_matrix <- cbind(
  data$annual_benchmark_price_lag1,
  data$annual_benchmark_price_lag2,
  data$annual_benchmark_price_lag3,
  data$annual_benchmark_price_lag4,
  data$annual_benchmark_price_lag5
)


# Use the new vectorized function:
tic()
data$arc_payment <- calc_arcco_payment_vectorized(
  crop = data$crop,
  crop_type = data$crop_type,
  program_year = data$program_year,
  actual_revenue = data$actual_revenue,
  base_acres = 1,
  mya_price = data$current_mya_price,
  srp = data$statutory_reference_price,
  oa_benchmark_yield = data$oa_bench_mark_yield,
  nmlr = data$current_national_loan_rate,
  historic_mya_prices = historical_prices_matrix,
  fips = data$fips,
  quiet = TRUE
)
toc()

# calculate the payments per base acre under the policy environment of the OBBB ------------------------

# Define OBBB policy parameters
new_srps <- list(
  "wheat" = 6.35,
  "oats" = 2.65,
  "rice" = .1690, # medium and long grain (temperate japonica handled separately)
  "cotton" = 0.42,
  "corn" = 4.10,
  "grain sorghum" = 4.40,
  "dry peas" = .131,
  "peanuts" = .315,
  "soybeans" = 10.00,
  "barley" = 5.45,
  "chickpeas_small" = .2265,
  "chickpeas_large" = .2565,
  "lentils" = .2375,
  "flaxseed" = 13.10,
  "canola" = .2375,
  "rapeseed" = .2375,
  "safflower" = .2375,
  "mustard" = .2375,
  "sunflower" = .2375,
  "sesame" = .2375,
  "crambe" = .2375
)

# Create base crop names for merging (remove type-specific suffixes)
srp_base_names <- names(new_srps)
srp_base_names[srp_base_names == "chickpeas_small"] <- "chickpeas"
srp_base_names[srp_base_names == "chickpeas_large"] <- "chickpeas"

srp_obbb <- data.frame(crop = srp_base_names, obbb_srps = unlist(new_srps)) %>%
  group_by(crop) %>%
  slice(1) %>% # Take first value for duplicated crops
  ungroup()

# Define OBBB loan rates
new_loan_rates <- list(
  "wheat" = 3.72,
  "oats" = 2.20,
  "rice" = .0770,
  "cotton" = 0.25,
  "corn" = 2.42,
  "grain sorghum" = 2.42,
  "dry peas" = 0.0687,
  "peanuts" = .195,
  "soybeans" = 6.82,
  "barley" = 2.75,
  "chickpeas_small" = .1100,
  "chickpeas_large" = .1540,
  "lentils" = .1430,
  "flaxseed" = .1110,
  "canola" = .1110,
  "rapeseed" = .1110,
  "safflower" = .1110,
  "mustard" = .1110,
  "sunflower" = .1110,
  "sesame" = .1110,
  "crambe" = .1110
)

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
data <- left_join(data, srp_obbb)
data <- left_join(data, nmlr_obbb)

# Add crop type specific rates using values from reference lists
data$obbb_srps[which(
  data$crop == "chickpeas" & data$crop_type == "small"
)] <- new_srps$chickpeas_small
data$obbb_srps[which(
  data$crop == "chickpeas" & data$crop_type == "large"
)] <- new_srps$chickpeas_large
data$obbb_srps[which(
  data$crop == "rice" & data$crop_type == "temperate japonica"
)] <- .1730

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

# Calculate OBBB PLC payments
tic()
data$plc_payment_obbb <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch(
    {
      # Extract historic MYA prices for this row
      historical_prices <- c(
        data$annual_benchmark_price_lag1[i],
        data$annual_benchmark_price_lag2[i],
        data$annual_benchmark_price_lag3[i],
        data$annual_benchmark_price_lag4[i],
        data$annual_benchmark_price_lag5[i]
      )

      # Calculate OBBB PLC payment
      result <- rfsa::calc_plc_payment(
        crop = data$crop[i],
        crop_type = data$crop_type[i],
        program_year = data$program_year[i],
        base_acres = 1,
        mya_price = data$current_mya_price[i],
        historic_mya_prices = historical_prices,
        srp = data$obbb_srps[i],
        erp = NULL,
        always_use_erp = FALSE,
        nmlr = data$obbb_nmlr[i],
        plc_yield = data$plc_yield[i],
        cov_lvl = 0.85,
        fips = data$fips[i],
        oa_pct = 0.88,
        cap = 1.15,
        quiet = TRUE
      )

      return(result)
    },
    error = function(e) {
      # Return NA on any error
      return(NA)
    }
  )
}))
toc()

# Calculate OBBB ARC-CO payments
tic()
data$arc_payment_obbb <- calc_arcco_payment_vectorized(
  crop = data$crop,
  crop_type = data$crop_type,
  program_year = data$program_year,
  actual_revenue = data$actual_revenue,
  base_acres = 1,
  mya_price = data$current_mya_price,
  srp = data$obbb_srps,
  oa_benchmark_yield = data$oa_bench_mark_yield,
  nmlr = data$obbb_nmlr,
  historic_mya_prices = historical_prices_matrix,
  fips = data$fips,
  quiet = TRUE,
  max_payment_level = 0.1,
  oa_pct = 0.88,
  cap = 1.15,
  payment_trigger_level = 0.9
)
toc()

# calculate payment per base acre (higher of provision)
data$higher_of_payment_obbb <- pmax(
  data$arc_payment_obbb,
  data$plc_payment_obbb,
  na.rm = TRUE
)
data$higher_of_payment <- pmax(data$arc_payment, data$plc_payment, na.rm = TRUE)

# merge in regions for plotting
# Load state-region mappings from CSV file
state_regions_df <- read.csv(
  system.file("extdata", "state_regions.csv", package = "title1Sim"),
  stringsAsFactors = FALSE
) %>%
  rename(state_name = state)
data <- left_join(data, state_regions_df, by = "state_name")


# add obbb effective reference prices
# add calculated effective reference prices (as a check)
data$obbb_erp <- unlist(lapply(1:nrow(data), function(i) {
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
        srp = data$obbb_srps[i],
        oa_pct = 0.88
      )
    },
    error = function(e) {
      # Return NA on any error
      return(NA)
    }
  )
}))


# load the cost of production data
load("data/ersCop.rda")
cost_data <- ersCop %>%
  filter(geography != "U.S. total", !grepl("total,",cost_item)) %>%
  filter(!is.na(cost_type)) %>%
  group_by(crop, fips, year, cost_type) %>%
  summarise(
    cost_per_acre = sum(cost_per_acre, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(crop, fips, year, cost_type, cost_per_acre) %>%
  pivot_wider(names_from = cost_type, values_from = cost_per_acre) %>%
  rename(fixed_cost_per_acre = "fixed" , variable_cost_per_acre = "variable") %>%
  mutate(fips = as.numeric(fips))

# merge with main data
data <- left_join(data, cost_data, by = c("crop", "fips", "program_year" = "year"))


cost_data_national <- ersCop %>%
  filter(geography == "U.S. total", !grepl("total,",cost_item)) %>%
  filter(!is.na(cost_type)) %>%
  group_by(crop, year, cost_type) %>%
  summarise(
    cost_per_acre = sum(cost_per_acre, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(crop, year, cost_type, cost_per_acre) %>%
  pivot_wider(names_from = cost_type, values_from = cost_per_acre) %>%
  rename(fixed_cost_per_acre_national = "fixed" , variable_cost_per_acre_national = "variable")

data <- left_join(data, cost_data_national, by = c("crop", "program_year" = "year"))


# attach ers resource regions as a column
# resource_regions <- ersCop %>%
#   filter(geography != "U.S. total") %>%
#   select(fips, geography) %>%
#   rename(ers_resource_region = geography) %>%
#   distinct() %>%
#   mutate(fips = as.numeric(fips))
#
# data <- left_join(data, resource_regions, by = "fips")

# calculate county revenue per acre
data$revenue_per_planted_acre <- data$actual_revenue
data$revenue_total <- data$actual_revenue * data$planted_acres
data$variable_cost_total <- data$variable_cost_per_acre * (data$planted_acres + data$failed_acres)
data$fixed_cost_total <- data$fixed_cost_per_acre * (data$planted_acres + data$failed_acres)
data$net_opperating_income_per_planted_acre <- data$revenue_per_planted_acre - data$variable_cost_per_acre
data$net_income_per_planted_acre <- data$revenue_per_planted_acre - data$fixed_cost_per_acre - data$variable_cost_per_acre

# convert any named vectors to unnammed
data$actual_yield <- as.numeric(data$actual_yield)
data$actual_revenue <- as.numeric(data$actual_revenue)
data$yield_imputed <- as.logical(data$yield_imputed)
data$revenue_per_planted_acre <- as.numeric(data$revenue_per_planted_acre)
data$revenue_total <- as.numeric(data$revenue_total)
data$net_opperating_income_per_planted_acre <- as.numeric(data$net_opperating_income_per_planted_acre)
data$net_income_per_planted_acre <- as.numeric(data$net_income_per_planted_acre)

# final data cleaning steps


# Export data as RDS file
saveRDS(data, "data-raw/simulation_data.rds")

# Export data as RDA file for package data
simulation_data <- data
usethis::use_data(simulation_data, overwrite = TRUE)

# check national levels
national_validation <- data.frame(year = 2019:2025, calc_plc = NA, act_plc = NA, plc_diff = NA, calc_arc = NA, act_arc = NA, arc_diff = NA)

for(y in national_validation$year){


  # plc
  try({
    national_validation$calc_plc[which(national_validation$year == y)] = sum(data$plc_payment[which(data$program_year == y)]*data[which(data$program_year == y),paste0("enrolled_base_PLC")]*(1-.068), na.rm = T)/1000000

    act = rfsa::get_fsa_payments(year = y, program = "PLC", year_type = "program")
    if(nrow(act) == 0){
      amount = 0
    } else {
      amount = act$payment_amount/1000000
    }
    national_validation$act_plc[which(national_validation$year == y)]  = amount
    national_validation$plc_diff[which(national_validation$year == y)] <- paste0(round((national_validation$calc_plc[which(national_validation$year == y)] - national_validation$act_plc[which(national_validation$year == y)])/national_validation$act_plc[which(national_validation$year == y)],2)*100,"%")
  })

  # arc
  try({
    national_validation$calc_arc[which(national_validation$year == y)] = sum(data$arc_payment[which(data$program_year == y)]*data[which(data$program_year == y),paste0("enrolled_base_ARCCO")]*(1-.068), na.rm = T)/1000000

    act = rfsa::get_fsa_payments(year = y, program = "ARC-CO", year_type = "program")
    if(nrow(act) == 0){
      amount = 0
    } else {
      amount = act$payment_amount/1000000
    }
    national_validation$act_arc[which(national_validation$year == y)]  = amount
    national_validation$arc_diff[which(national_validation$year == y)] <- paste0(round((national_validation$calc_arc[which(national_validation$year == y)] - national_validation$act_arc[which(national_validation$year == y)])/national_validation$act_arc[which(national_validation$year == y)],2)*100,"%")
  })


}
national_validation

national_validation[national_validation == "NaN%"] <- "0%"





