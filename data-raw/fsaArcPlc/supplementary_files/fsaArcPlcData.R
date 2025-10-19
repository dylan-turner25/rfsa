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

# Note: at this point, base acres are duplicated for irrigation status, i.e. if irrigated and non-irrigated yields are reported,
# base acres are duplicated since we don't have irrigation status in the base acres data. RaFF addressed this by assuming all acres were non-irrigated
# and using the census of agriculture to estimate the share of irrigated acres by county and applying base acres proportionally. This could be done in the future,
# in which case the code to do so would be placed here. For now, we are assuming non-irrigated and filtering out irrigated observations.

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

  oats = "OATS"
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



