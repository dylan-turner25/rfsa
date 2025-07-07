
library(tidyverse)
library(tictoc)

devtools::load_all()

data("fsaArcCoBenchmarks")
data("fsaMyaPrice")
data("fsaPlcYields")
data("fsaEffectiveRefPrices")
data("fsaArcCoPrice")
data("fsaCountyBaseAcres")
data("fsaEnrolledCountyBaseAcres")

# start with arc co benchmarks
data <- fsaArcCoBenchmarks %>%
  mutate(marketing_year = paste0(program_year,"-",program_year + 1))

# merge in arc-co prices
arc_co_price <- fsaArcCoPrice %>%
  select(crop, contains("benchmark_price"),
         current_mya_price, current_national_loan_rate,
         marketing_year, program_year, crop_type)
data <- left_join(data, arc_co_price)

# merge in mya prices
prices <- fsaMyaPrice %>%
  select(crop, crop_type, marketing_year, contains("mya_price"), -contains("publishing"))
data <- left_join(data, prices)

# merge in plc yield
plc_yields <- fsaPlcYields %>%
  select(fips,crop,crop_type, plc_yield, program_year)
data <- left_join(data, plc_yields)

# Fill missing PLC yields for program years before 2018 with 2018 values
data <- data %>%
  group_by(fips, crop, crop_type) %>%
  mutate(plc_yield = ifelse(program_year < 2018 & is.na(plc_yield),
                           plc_yield[program_year == 2018][1],
                           plc_yield)) %>%
  ungroup()


# add statutory_reference_prices
srp <- distinct(fsaEffectiveRefPrices %>%
                 select(statutory_reference_price, crop, crop_type))
data <- left_join(data, srp)

# add observed effective reference prices
erp   <- distinct(fsaEffectiveRefPrices %>%
                   select(effective_reference_price, crop, crop_type,
                          marketing_year, program_year))
data <- left_join(data, erp)

# add calculated effective reference prices (as a check)
data$erp_calc <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch({
    # Extract MYA prices for this row
    mya_prices <- c(data$final_mya_price_lag2[i],
                    data$final_mya_price_lag3[i],
                    data$final_mya_price_lag4[i],
                    data$final_mya_price_lag5[i],
                    data$final_mya_price_lag6[i])


    # Calculate ERP
    rfsa:::calc_effective_reference_price(mya_prices = mya_prices,
                                          srp = data$statutory_reference_price[i])
  }, error = function(e) {
    # Return NA on any error
    return(NA)
  })
}))

data$erp_calc_check <- as.numeric(data$erp_calc == data$effective_reference_price)
summary(data$erp_calc_check)

# add calculated benchmark price (vectorized with lapply)
data$oa_bench_mark_price_calc <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch({
    # Extract historical benchmark prices for this row
    historical_prices <- c(data$annual_benchmark_price_lag1[i],
                          data$annual_benchmark_price_lag2[i],
                          data$annual_benchmark_price_lag3[i],
                          data$annual_benchmark_price_lag4[i],
                          data$annual_benchmark_price_lag5[i])


    # Calculate Olympic average benchmark price
    result <- rfsa:::get_arcco_benchmarks(crop = data$crop[i],
                                  program_year = data$program_year[i],
                                  benchmark_type = "price",
                                  erp = max(data$effective_reference_price[i],data$statutory_reference_price[i], na.rm = T),
                                  crop_type = data$crop_type[i],
                                  historical_prices = historical_prices,
                                  fips = data$fips[i],
                                  quiet = TRUE)

    return(round(result, 2))
  }, error = function(e) {
    # Return NA on any error
    return(NA)
  })
}))

# Tabulate missing values by program year
missing_table <- table(data$program_year, is.na(data$oa_bench_mark_price_calc), useNA = "ifany")
colnames(missing_table) <- c("Complete", "Missing")
print(missing_table)

# accuracy check on benchmark price
data$oa_bench_mark_price_check <- as.numeric(data$oa_bench_mark_price_calc == data$oa_bench_mark_price)
summary(data$oa_bench_mark_price_check)

# # merge in total base acres (current year)
base <- fsaCountyBaseAcres %>%
  group_by(fips, program_year, crop, crop_type) %>%
  summarize(base_acres = sum(base_acres), .groups = "drop")

data <- left_join(data, base)

# # merge in enrolled base acres (current year)
enrolled_base <- fsaEnrolledCountyBaseAcres %>%
  select(fips, program_year, crop, crop_type, contains("enrolled"))

data <- left_join(data, enrolled_base)

# # create pivoted base acres by year (all years as columns)
# First check data quality
base_acres_summary <- fsaCountyBaseAcres %>%
  group_by(program_year) %>%
  summarise(
    total_records = n(),
    valid_fips = sum(!is.na(fips)),
    na_fips = sum(is.na(fips)),
    .groups = "drop"
  )

cat("Base acres data quality by year:\n")
print(base_acres_summary)

# # Filter out records with missing FIPS before pivot
base_acres_wide <- fsaCountyBaseAcres %>%
  group_by(fips, program_year, crop, crop_type) %>%
  summarize(base_acres = sum(base_acres), .groups = "drop") %>%
  pivot_wider(
    names_from = program_year,
    values_from = base_acres,
    names_prefix = "base_acres_",
    values_fill = 0
  )

data <- left_join(data, base_acres_wide)

# Report on join success
cat("Base acres wide join summary:\n")
cat("Records in data:", nrow(data), "\n")
cat("Records with base_acres_2023 > 0:", sum(data$base_acres_2023 > 0, na.rm = TRUE), "\n")

# create pivoted enrolled base acres by year (all years as columns)
# Check enrolled base data quality
enrolled_summary <- fsaEnrolledCountyBaseAcres %>%
  group_by(program_year) %>%
  summarise(
    total_records = n(),
    valid_fips = sum(!is.na(fips)),
    na_fips = sum(is.na(fips)),
    .groups = "drop"
  )

cat("Enrolled base acres data quality by year:\n")
print(enrolled_summary)

# Get enrolled columns and filter out missing FIPS
enrolled_cols <- fsaEnrolledCountyBaseAcres %>%
  select(contains("enrolled")) %>%
  colnames()


enrolled_base_wide <- fsaEnrolledCountyBaseAcres %>%
  select(fips, program_year, crop, crop_type, all_of(enrolled_cols)) %>%
  pivot_wider(
    names_from = program_year,
    values_from = all_of(enrolled_cols),
    names_sep = "_",
    values_fill = list(.default = 0),
    values_fn = sum
  )

data <- left_join(data, enrolled_base_wide)


# calculate plc payment per base acres
tic()
data$plc_payment <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch({
    # Extract historic MYA prices for this row
       historical_prices <- c(data$annual_benchmark_price_lag1[i],
                          data$annual_benchmark_price_lag2[i],
                          data$annual_benchmark_price_lag3[i],
                          data$annual_benchmark_price_lag4[i],
                          data$annual_benchmark_price_lag5[i])



    # Calculate PLC payment
    result <- rfsa::calc_plc_payment(crop = data$crop[i],
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
                              quiet = TRUE)

    return(result)
  }, error = function(e) {
    # Return NA on any error
    return(NA)
  })
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
data$arc_payment <- calc_arcco_payment_vectorized(
  crop = data$crop,
  crop_type = data$crop_type,
  program_year = data$program_year,
  base_acres = 1,
  mya_price = data$current_mya_price,
  srp = data$statutory_reference_price,
  oa_benchmark_yield = data$oa_bench_mark_yield,
  nmlr = data$current_national_loan_rate,
  historic_mya_prices = historical_prices_matrix,
  fips = data$fips,
  quiet = TRUE
)




# apply policy changes from OBBB
new_srps <- list("wheat" = 6.35,
                 "oats" = 2.65,
                 "rice" = .1690,  # medium and long grain (temperate japonica handled separately)
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

srp_obbb <- data.frame(crop = names(new_srps), obbb_srps = unlist(new_srps))

# merge new srps with data
data <- left_join(data, srp_obbb)

# calculate arc and plc payments again using the new obbb parameters


# calculate new effective reference prices based on new erp parameters


# PLC parameters
obbb_cov_lvl = .88


# calculate plc payment per base acres
tic()
data$plc_payment_obbb <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch({
    # Extract historic MYA prices for this row
    historical_prices <- c(data$annual_benchmark_price_lag1[i],
                           data$annual_benchmark_price_lag2[i],
                           data$annual_benchmark_price_lag3[i],
                           data$annual_benchmark_price_lag4[i],
                           data$annual_benchmark_price_lag5[i])



    # Calculate PLC payment
    result <- rfsa::calc_plc_payment(crop = data$crop[i],
                                     crop_type = data$crop_type[i],
                                     program_year = data$program_year[i],
                                     base_acres = 1,
                                     mya_price = data$current_mya_price[i],
                                     historic_mya_prices = historical_prices,
                                     srp = data$obbb_srps[i],
                                     erp = NULL,
                                     always_use_erp = FALSE,
                                     nmlr = data$current_national_loan_rate[i],
                                     plc_yield = data$plc_yield[i],
                                     cov_lvl = obbb_cov_lvl,
                                     fips = data$fips[i],
                                     quiet = TRUE)

    return(result)
  }, error = function(e) {
    # Return NA on any error
    return(NA)
  })
}))
toc()



# ARC-CO parameters
obbb_max_payment_level = 0.1
payment_trigger_level = 0.9




# Prepare vectorized inputs for optimized function
historical_prices_matrix <- cbind(
  data$annual_benchmark_price_lag1,
  data$annual_benchmark_price_lag2,
  data$annual_benchmark_price_lag3,
  data$annual_benchmark_price_lag4,
  data$annual_benchmark_price_lag5
)

# Use the new vectorized function:
data$arc_payment <- calc_arcco_payment_vectorized(
  crop = data$crop,
  crop_type = data$crop_type,
  program_year = data$program_year,
  base_acres = 1,
  mya_price = data$current_mya_price,
  srp = data$statutory_reference_price,
  oa_benchmark_yield = data$oa_bench_mark_yield,
  nmlr = data$current_national_loan_rate,
  historic_mya_prices = historical_prices_matrix,
  fips = data$fips,
  quiet = TRUE
)











# check national levels
national_validation <- data.frame(year = 2014:2025, calc_plc = NA, act_plc = NA, plc_diff = NA, calc_arc = NA, act_arc = NA, arc_diff = NA)

for(y in national_validation$year){

  print(y)

  # plc
  try({
  national_validation$calc_plc[which(national_validation$year == y)] = sum(data$plc_payment[which(data$program_year == y)]*data$enrolled_base_PLC_2020[which(data$program_year == y)], na.rm = T)/1000000

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
    national_validation$calc_arc[which(national_validation$year == y)] = sum(data$arc_payment[which(data$program_year == y)]*data$enrolled_base_ARCCO_2020[which(data$program_year == y)], na.rm = T)/1000000

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











