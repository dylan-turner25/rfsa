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

# convert to a tibble before exporting
fsaEnrolledCountyBaseAcres <- dplyr::as_tibble(enrolled_county_base_acres)

# use the county level file in the package data folder
usethis::use_data(fsaEnrolledCountyBaseAcres, overwrite = TRUE)
