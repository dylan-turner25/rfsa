# Source helper functions
source("./R/helpers.R")

# Load required libraries
library(dplyr)
library(readxl)
library(janitor)
library(readr)
library(usethis)

# list all files in "./data-raw/fsaArcPlc/input_data/fsaEnrolledCountyBaseAcres"
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaEnrolledCountyBaseAcres",
                    full.names = TRUE, pattern = "\\.xlsx$")

# initialize a data frame to store enrolled county base acres
enrolled_county_base_acres <- NULL

# load and process each Excel file
for(file in files){
  # extract year from filename
  year <- as.numeric(gsub(".*([0-9]{4}).*", "\\1", basename(file)))

  # read the first sheet of each file
  temp <- readxl::read_excel(file, sheet = 1)

  # handle different file formats based on year
  if(year <= 2021){
    # 2019-2021 format: standard headers
    temp <- janitor::clean_names(temp)
    names(temp) <- c("st_cty", "state_name", "county_name", "crop_name", "program", "enrolled_base")
  } else {
    # 2022+ format: headers in row 2, need to skip first row
    temp <- temp[-1, ]  # remove first row
    names(temp) <- c("st_cty", "state_name", "county_name", "crop_name", "program", "enrolled_base")
    temp <- temp[-1, ]  # remove the header row
  }

  # remove any rows with all NA values
  temp <- temp[rowSums(is.na(temp)) != ncol(temp), ]

  # add program year
  temp$program_year <- year

  # ensure enrolled_base is numeric
  temp$enrolled_base <- as.numeric(temp$enrolled_base)

  # type convert other columns
  temp <- readr::type_convert(temp)

  # bind to master data frame
  enrolled_county_base_acres <- dplyr::bind_rows(enrolled_county_base_acres, temp)
}

# extract crop type first (before cleaning crop names)
enrolled_county_base_acres$crop_type <- unlist(lapply(enrolled_county_base_acres$crop_name, extract_crop_type))

# add rma crop codes where applicable (before cleaning crop names)
enrolled_county_base_acres$rma_type_code <- unlist(lapply(enrolled_county_base_acres$crop_name, extract_crop_type, rma_code = TRUE))

# clean crop names (this removes crop type suffixes after extraction)
enrolled_county_base_acres$crop_name <- unlist(lapply(enrolled_county_base_acres$crop_name, clean_crop_names2))

# add rma crop codes (after cleaning)
enrolled_county_base_acres$rma_crop_code <- unlist(lapply(enrolled_county_base_acres$crop_name, assign_rma_cc))

# rename columns for consistency
enrolled_county_base_acres <- enrolled_county_base_acres %>%
  dplyr::rename(crop = crop_name,
                state = state_name,
                county = county_name,
                fips = st_cty)

# pivot wider to create separate columns for each program type
enrolled_county_base_acres <- enrolled_county_base_acres %>%
  tidyr::pivot_wider(
    names_from = program,
    values_from = enrolled_base,
    names_prefix = "enrolled_base_",
    values_fill = 0
  )

# collapse county-name variants that share a fips. When FSA renames a county
# office it can list the county under both names for a few years with the same
# acres (Henrico-Richmond City / Henrico VA and Snohomish-North King /
# Snohomish WA in 2023 and 2024). Those rows are true duplicates, so keep one,
# labelled with the name the fips uses in its latest program year. Variants
# whose acres differ are not duplicates we know how to resolve, so stop.
key_cols <- c("fips", "program_year", "crop", "crop_type")
enrolled_cols <- grep("^enrolled_base_", names(enrolled_county_base_acres), value = TRUE)

latest_names <- enrolled_county_base_acres %>%
  dplyr::filter(program_year == max(program_year), .by = fips) %>%
  dplyr::distinct(fips, latest_county = county) %>%
  dplyr::slice(1, .by = fips)

enrolled_county_base_acres <- enrolled_county_base_acres %>%
  dplyr::left_join(latest_names, by = "fips", relationship = "many-to-one") %>%
  dplyr::arrange(county != latest_county) %>%
  dplyr::distinct(dplyr::across(dplyr::all_of(c(key_cols, enrolled_cols))),
                  .keep_all = TRUE) %>%
  dplyr::select(-latest_county)

dup_keys <- enrolled_county_base_acres %>%
  dplyr::count(dplyr::across(dplyr::all_of(key_cols))) %>%
  dplyr::filter(n > 1)
if (nrow(dup_keys) > 0) {
  print(dup_keys)
  stop("fsaEnrolledCountyBaseAcres: ", nrow(dup_keys), " fips-year-crop keys ",
       "have more than one row with different enrolled acres")
}

# convert to a tibble before exporting
fsaEnrolledCountyBaseAcres <- dplyr::as_tibble(enrolled_county_base_acres)

# use the county level file in the package data folder
usethis::use_data(fsaEnrolledCountyBaseAcres, overwrite = TRUE)
