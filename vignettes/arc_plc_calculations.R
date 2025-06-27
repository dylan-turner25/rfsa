
library(dplyr)
library(tictoc)

devtools::load_all()

data("fsaArcCoBenchmarks")
data("fsaMyaPrice")
data("fsaPlcYields")
data("fsaEffectiveRefPrices")
data("fsaArcCoPrice")


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



# aggregate to the state year level
data <- data %>%
  group_by(state_name, crop, yield_type, program_year,crop_type, rma_type_code, rma_crop_code, marketing_year) %>%
  select(-fips, -contains("calc"),-contains("check"), -county_name) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))


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

# Tabulate missing values by program year
missing_plc_table <- table(data$program_year, is.na(data$plc_payment), useNA = "ifany")
colnames(missing_plc_table) <- c("Complete", "Missing")
print(missing_plc_table)

# calculate arc-co payment per base acres (vectorized with lapply)
cat("Calculating ARC-CO payments for", nrow(data), "records...\n")
tic()
data$arcco_payment <- unlist(lapply(1:nrow(data), function(i) {
  tryCatch({
    # Extract historic MYA prices for this row
    historical_prices <- c(data$annual_benchmark_price_lag1[i],
                           data$annual_benchmark_price_lag2[i],
                           data$annual_benchmark_price_lag3[i],
                           data$annual_benchmark_price_lag4[i],
                           data$annual_benchmark_price_lag5[i])


    # Calculate ARC-CO payment
    result <- calc_arcco_payment(crop = data$crop[i],
                                crop_type = data$crop_type[i],
                                program_year = data$program_year[i],
                                base_acres = 1,
                                mya_price = data$current_mya_price[i],
                                srp = data$statutory_reference_price[i],
                                oa_benchmark_price = 5,
                                oa_benchmark_yield = data$oa_bench_mark_yield[i],
                                erp = NULL,
                                nmlr = data$current_national_loan_rate[i],
                                historic_mya_prices = historical_prices,
                                fips = data$fips[i],
                                quiet = TRUE)

    return(result)
  }, error = function(e) {
    # Return NA on any error
    return(NA)
  })
}))
toc()



#
# # Tabulate missing values by program year for ARC-CO
# missing_arcco_table <- table(data$program_year, is.na(data$arcco_payment), useNA = "ifany")
# colnames(missing_arcco_table) <- c("Complete", "Missing")
# print(missing_arcco_table)



