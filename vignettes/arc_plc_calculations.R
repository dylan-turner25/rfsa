
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
# base <- fsaCountyBaseAcres %>%
#   group_by(fips, program_year, crop, crop_type) %>%
#   summarize(base_acres = sum(base_acres), .groups = "drop")
#
# data <- left_join(data, base)
#
# # merge in enrolled base acres (current year)
# enrolled_base <- fsaEnrolledCountyBaseAcres %>%
#   select(fips, program_year, crop, crop_type, contains("enrolled"))
#
# data <- left_join(data, enrolled_base)
#
# # create pivoted base acres by year (all years as columns)
# # First check data quality
# base_acres_summary <- fsaCountyBaseAcres %>%
#   group_by(program_year) %>%
#   summarise(
#     total_records = n(),
#     valid_fips = sum(!is.na(fips)),
#     na_fips = sum(is.na(fips)),
#     .groups = "drop"
#   )
#
# cat("Base acres data quality by year:\n")
# print(base_acres_summary)
#
# # Filter out records with missing FIPS before pivot
# base_acres_wide <- fsaCountyBaseAcres %>%
#   group_by(fips, program_year, crop, crop_type) %>%
#   summarize(base_acres = sum(base_acres), .groups = "drop") %>%
#   pivot_wider(
#     names_from = program_year,
#     values_from = base_acres,
#     names_prefix = "base_acres_",
#     values_fill = 0
#   )
#
# data <- left_join(data, base_acres_wide)
#
# # Report on join success
# cat("Base acres wide join summary:\n")
# cat("Records in data:", nrow(data), "\n")
# cat("Records with base_acres_2023 > 0:", sum(data$base_acres_2023 > 0, na.rm = TRUE), "\n")
#
# # create pivoted enrolled base acres by year (all years as columns)
# # Check enrolled base data quality
# enrolled_summary <- fsaEnrolledCountyBaseAcres %>%
#   group_by(program_year) %>%
#   summarise(
#     total_records = n(),
#     valid_fips = sum(!is.na(fips)),
#     na_fips = sum(is.na(fips)),
#     .groups = "drop"
#   )
#
# cat("Enrolled base acres data quality by year:\n")
# print(enrolled_summary)
#
# # Get enrolled columns and filter out missing FIPS
# enrolled_cols <- fsaEnrolledCountyBaseAcres %>%
#   select(contains("enrolled")) %>%
#   colnames()
#
# enrolled_base_wide <- fsaEnrolledCountyBaseAcres %>%
#   select(fips, program_year, crop, crop_type, all_of(enrolled_cols)) %>%
#   pivot_wider(
#     names_from = program_year,
#     values_from = all_of(enrolled_cols),
#     names_sep = "_",
#     values_fill = list(.default = 0)
#   )
#
# data <- left_join(data, enrolled_base_wide)





# aggregate to the state year level
# data <- data %>%
#   group_by(state_name, crop, yield_type, program_year,crop_type, rma_type_code, rma_crop_code, marketing_year) %>%
#   select(-fips, -contains("calc"),-contains("check"), -county_name) %>%
#   summarise_all(funs(mean(., na.rm = TRUE)))
#

# calculate plc payment per base acres (vectorized with lapply)
cat("Calculating PLC payments for", nrow(data), "records...\n")
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


# average payment per acre by crop
plc_payment_per_acre <- data %>%
  group_by(program_year, crop, crop_type) %>%
  summarise(plc_payment = mean(plc_payment, na.rm = T))


# Tabulate missing values by program year
missing_plc_table <- table(data$program_year, is.na(data$plc_payment), useNA = "ifany")
colnames(missing_plc_table) <- c("Complete", "Missing")
print(missing_plc_table)




# Calculate using original method for comparison
original_payments <- sapply(1:10, function(i) {
  tryCatch({

    historical_prices <- c(data$annual_benchmark_price_lag1[i],
                          data$annual_benchmark_price_lag2[i],
                          data$annual_benchmark_price_lag3[i],
                          data$annual_benchmark_price_lag4[i],
                          data$annual_benchmark_price_lag5[i])

    calc_arcco_payment(crop = data$crop[i],
                      crop_type = data$crop_type[i],
                      program_year = data$program_year[i],
                      base_acres = 1,
                      mya_price = data$current_mya_price[i],
                      srp = data$statutory_reference_price[i]*1,
                      oa_benchmark_price = NULL,
                      oa_benchmark_yield = data$oa_bench_mark_yield[i],
                      erp = NULL,
                      nmlr = data$current_national_loan_rate[i],
                      historic_mya_prices = historical_prices,
                      fips = data$fips[i],
                      quiet = TRUE)

  }, error = function(e) NA)
})






#
# # Tabulate missing values by program year for ARC-CO
# missing_arcco_table <- table(data$program_year, is.na(data$arcco_payment), useNA = "ifany")
# colnames(missing_arcco_table) <- c("Complete", "Missing")
# print(missing_arcco_table)

data("fsaMyaPrice")
data("fsaArcCoBenchmarks")
data("fsaEffectiveRefPrices")

results <- data.frame(crop = NULL, program_year = NULL,
                      arc_payment = NULL, plc_payment = NULL,
                      arc_payment_obbb = NULL, plc_payment_obbb = NULL)

for(c in c("wheat", "oats", "corn",
            "soybeans", "barley", "canola")){
  for(py in 2014:2023){

# define crop
crop = c

# define program year
program_year = py
srp = unique(fsaEffectiveRefPrices %>%
  filter(.data$crop == .env$crop) %>%
  pull(statutory_reference_price))

# define historic prices
if(py <= 2018){
  plc_historic_prices <- t(data.frame(fsaMyaPrice %>%
                                        filter(.data$crop == .env$crop,
                                               .data$marketing_year == paste0(.env$program_year,"-",.env$program_year + 1)) %>%
                                        select(contains("lag")) %>% select(-contains("lag6"))))[,1]
} else {
plc_historic_prices <- t(data.frame(fsaMyaPrice %>%
  filter(.data$crop == .env$crop,
         .data$marketing_year == paste0(.env$program_year,"-",.env$program_year + 1)) %>%
  select(contains("lag")) %>% select(-contains("lag1"))))[,1]
}
arc_historic_prices <- t(data.frame(fsaArcCoPrice %>%
  filter(.data$crop == .env$crop,
         .data$program_year == .env$program_year) %>%
  select(contains("lag"))))[,1]

# calculate plc payment
plc_payment <- calc_plc_payment(crop = crop,
                 program_year = program_year,
                 srp = srp,
                 historic_mya_prices = plc_historic_prices)

# calculate arc_payment
arc_payment <- calc_arcco_payment(crop = "corn",
                   program_year = program_year,
                   srp = srp,
                   historic_mya_prices = arc_historic_prices)

# apply policy change
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


srp = new_srps[c][[1]]


# calculate plc payment
plc_payment_obbb <- calc_plc_payment(crop = crop,
                                program_year = program_year,
                                srp = srp,
                                historic_mya_prices = plc_historic_prices)

# calculate arc_payment
arc_payment_obbb <- calc_arcco_payment(crop = "corn",
                                  program_year = program_year,
                                  srp = srp,
                                  payment_trigger_level = .9,
                                  max_payment_level = .125,
                                  historic_mya_prices = arc_historic_prices)


results <- rbind.data.frame(results, data.frame(crop = crop,
                                                program_year = program_year,
                                                arc_payment = arc_payment,
                                                plc_payment = plc_payment,
                                                arc_payment_obbb = arc_payment_obbb,
                                                plc_payment_obbb = plc_payment_obbb))
  }
}

results$arc_diff <- results$arc_payment_obbb - results$arc_payment
results$plc_diff <- results$plc_payment_obbb - results$plc_payment

# merge in total base acres
data("fsaArcPlcBaseAcres")
ba <- fsaArcPlcBaseAcres %>%
  select(covered_commodity, program_year, total, plc_total, arc_co_total, arc_ic_total) %>%
  rename(crop = covered_commodity,
         base_total = total,
         base_plc = plc_total,
         base_arcco = arc_co_total,
         base_arcic = arc_ic_total)

results <- left_join(results, ba)

# calculate total payments
results$arc_payment_tot <- results$arc_payment*results$base_arcco
results$plc_payment_tot <- results$plc_payment*results$base_plc


# add arc plc total payments
payments <- rfsa::get_fsa_payments(year = 2014:2023, program = c("ARC-CO","ARC-IC","PLC")) %>%
  select(-year_type, -program_name) %>%
  rename(program_year = year) %>%
  pivot_wider(names_from = program_abb, values_from = payment_amount) %>%
  replace(is.na(.), 0)

results <- left_join(results, payments)



results$arc_co_payment_per_acre = results$`ARC-CO`/results$base_arcco
results$plc_payment_per_acre = results$PLC/results$base_plc

# average over time
avg <- results %>% group_by(crop) %>% summarize_all(mean)





