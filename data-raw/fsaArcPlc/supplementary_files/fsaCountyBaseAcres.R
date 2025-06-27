# Load required packages and functions
library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(purrr)
library(fipio)
library(usethis)

# Source helper functions
source("R/helpers.R")

# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaCountyBaseAcres",
                    full.names = T)

# initialize a data frame to store arc-ic prices
county_base_acres <- NULL

# load data files
data_2014 <- readxl::read_excel(files[grepl("2014", files)], sheet = 2)
data_2015 <- readxl::read_excel(files[grepl("2014", files)], sheet = 3)
data_2021 <- readxl::read_excel(files[grepl("2021", files)], sheet = 2)
data_2022 <- readxl::read_excel(files[grepl("2022", files)], sheet = 2)
data_2023 <- readxl::read_excel(files[grepl("2023", files)], sheet = 2)

# clean 2014 and 2015
for(y in 2014:2015){

  # assign df to temporary object
  temp <- eval(parse(text = paste0("data_", y)))

  # clean names using janitor
  temp <- janitor::clean_names(temp)

  # add a program year column
  temp$program_year <- y

  assign(paste0("data_", y), temp)

}

# clean 2021, 2022, and 2023
for(y in 2021:2023){
  # assign df to temporary object
  temp <- eval(parse(text = paste0("data_", y)))

  # clean names using janitor
  temp <- janitor::clean_names(temp)

  # The actual column names after janitor::clean_names are different
  # Based on the raw data: "Program Year", "Admin State", "Admin County", "Crop Base Name", "Crop Base Acres"
  # After clean_names: "program_year", "admin_state", "admin_county", "crop_base_name", "crop_base_acres"
  temp <- temp %>%
    rename(
      state = admin_state,
      county = admin_county,
      crop = crop_base_name,
      base_acres = crop_base_acres
    ) %>%
    # Standardize case to match 2014-2015 data (ALL CAPS)
    mutate(
      state = str_to_upper(state),
      county = str_to_upper(county)
    )

  assign(paste0("data_", y), temp)

}

# combine all data frames
county_base_acres <- dplyr::bind_rows(data_2014, data_2015, data_2021, data_2022, data_2023)

# extract crops codes
county_base_acres$extracted_crop_type <- unlist(lapply(county_base_acres$crop, extract_crop_type))
county_base_acres$extracted_crop_type2 <- unlist(lapply(paste0( county_base_acres$crop," ",county_base_acres$crop_type), extract_crop_type))

county_base_acres <- county_base_acres %>%
  mutate(crop_type = case_when(
    !is.na(extracted_crop_type) ~ extracted_crop_type,
    !is.na(extracted_crop_type2) ~ extracted_crop_type2,
    TRUE ~ NA_character_
  ))

# add rma crop codes where applicable
county_base_acres$rma_type_code <- unlist(lapply(paste0(county_base_acres$crop," ",county_base_acres$crop_type ), extract_crop_type, rma_code = TRUE))

# clean crop names
county_base_acres$crop <- unlist(lapply(county_base_acres$crop, clean_crop_names2))

# add rma crop codes
county_base_acres$rma_crop_code <- unlist(lapply(county_base_acres$crop, assign_rma_cc))

# drop any unneeded columns
county_base_acres <- county_base_acres %>%
  select(-extracted_crop_type, -extracted_crop_type2)

# get fips codes for each state-county pair
library(dplyr)
library(stringr)
library(purrr)
library(fipio)

# Use usmap package for more reliable FIPS lookup
library(usmap)

# Create FIPS lookup using built-in county data
fips_reference <- usmap::us_map(regions = "counties") %>%
  select(fips, full, county) %>%
  distinct() %>%
  mutate(
    state_name = str_to_upper(full),
    county_name = str_to_upper(str_remove(county, " County| Parish| Borough| Census Area"))
  ) %>%
  select(state_name, county_name, fips)

unique_fips <- county_base_acres %>%
  filter(!is.na(state)) %>%
  distinct(state, county) %>%
  mutate(
    # Standardize state and county names before lookup
    state_clean = str_to_upper(str_trim(state)),
    county_clean = str_to_upper(str_trim(county))
  ) %>%
  # Join with reference table
  left_join(fips_reference, by = c("state_clean" = "state_name", "county_clean" = "county_name")) %>%
  # For unmatched records, add manual fixes for common issues
  mutate(
    fips = case_when(
      # Use matched FIPS if available
      !is.na(fips) ~ fips,
      # Manual overrides for known mismatches
      state_clean == "ILLINOIS" & county_clean == "DEWITT" ~ "19045", 
      state_clean == "MAINE" & county_clean %in% c("FORT KENT", "HOULTON") ~ "23003",
      # Add more common county name variations
      state_clean == "ALASKA" & county_clean == "ALEUTIANS EAST" ~ "02013",
      state_clean == "ALASKA" & county_clean == "ALEUTIANS WEST" ~ "02016",
      TRUE ~ fips  # Keep the original matched value or NA
    ),
    # Keep cleaned versions for consistency  
    state = state_clean,
    county = county_clean
  ) %>%
  select(state, county, fips) %>%
  distinct()

# merge the fips codes back into the main data frame
cat("Before merge - sample county_base_acres (2023):\n")
print(head(county_base_acres[county_base_acres$program_year == 2023, c("state", "county")], 3))

cat("Sample unique_fips:\n")
print(head(unique_fips, 3))

cat("Merging FIPS codes...\n")
county_base_acres <- dplyr::left_join(county_base_acres, unique_fips, by = c("state", "county"))

cat("After merge - sample county_base_acres (2023):\n")
print(head(county_base_acres[county_base_acres$program_year == 2023, c("state", "county", "fips")], 3))

# Check merge success
merge_success <- county_base_acres %>%
  filter(program_year %in% 2021:2023) %>%
  summarise(
    total_records = n(),
    with_fips = sum(!is.na(fips)),
    success_rate = round(100 * with_fips / total_records, 1)
  )

cat("Merge success for 2021-2023:\n")
print(merge_success)

# convert to a tibble before exporting
fsaCountyBaseAcres <- dplyr::as_tibble(county_base_acres)


# use the county level file in the package data folder
usethis::use_data(fsaCountyBaseAcres , overwrite = TRUE)



