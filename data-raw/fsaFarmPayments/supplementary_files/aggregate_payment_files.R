# get a vector of all the cleaned individual rds files
rds_files <- list.files( path = "./data-raw/fsaFarmPayments/input_data", pattern = "*cleaned.rds", full.names = TRUE )

# build a single giant data frame of all individual payments
data <- map_dfr(rds_files, readRDS)

colnames(data) <- c("state_cd_fsa","state_name_fsa","county_cd_fsa","county_name_fsa",
                    "name_payee","address_info","address_payee","city_payee","state_ab_payee","zip_payee",
                    "delivery_point_bar_code","payment_amount","payment_date","accounting_program_code",
                    "accounting_program_description","program_year","payment_year","fiscal_year",
                    "program_abb","program_name","fips_fsa")

# aggregate into a county level data set by program year
fsaFarmPaymentsProgramYear <- data %>%
  group_by(fips_fsa, state_cd_fsa, county_cd_fsa, county_name_fsa,
           program_year, program_abb, program_name) %>%
  summarise(payment_amount = sum(payment_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(year = program_year) %>%
  mutate(year_type = "program")

usethis::use_data(fsaFarmPaymentsProgramYear, overwrite = TRUE)

# aggregate into a county level data set by payment year
fsaFarmPaymentsPaymentYear <- data %>%
  group_by(fips_fsa, state_cd_fsa, county_cd_fsa, county_name_fsa,
           payment_year, program_abb, program_name) %>%
  summarise(payment_amount = sum(payment_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(year = payment_year) %>%
  mutate(year_type = "payment")

usethis::use_data(fsaFarmPaymentsPaymentYear, overwrite = TRUE)

# aggregate into a county level data set by fiscal year
fsaFarmPaymentsFiscalYear <- data %>%
  group_by(fips_fsa, state_cd_fsa, county_cd_fsa, county_name_fsa,
           fiscal_year, program_abb, program_name) %>%
  summarise(payment_amount = sum(payment_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(year = fiscal_year) %>%
  mutate(year_type = "fiscal")

usethis::use_data(fsaFarmPaymentsFiscalYear, overwrite = TRUE)

