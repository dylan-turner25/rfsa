## code to prepare `fsaFarmPayments` dataset goes here

# load libraries
library(tidyverse)
library(readxl)
library(data.table)


# First run download_payment_files.R to download each of the individual farm payment excel files from the FSA website
source("./data-raw/fsaFarmPayments/supplementary_files/download_payment_files.R")

# merge all available info on FSA program details, by sourcing the following script which generates
# the fsaProgramDetails data set which can be loaded with `data("fsaProgramDetails")`
#isource("./data-raw/fsaFarmPayments/Supplementary Files/fsaProgramClassifications/fsaProgramClassifications.R")

# Second, run get_program_descriptions.R to create an excel file of unique program descriptions.
# This script will use the data set `fsaProgramDetails` created above to try to classify each
# payment and will output "program_detail_final.csv". The script will issue an error message
# if there are new payment descriptions that are not captured in the fsaProgramDetails data set and the
# script will append them to  "program_detail_manual.csv.In that case,
# you need to open program_detail_manual.csv and manually update any missing program classifications.
source("./data-raw/fsaFarmPayments/supplementary_files/get_program_descriptions.R")

# Third, run this file to apply some cleaning operations, then convert each individual payment file to rds files
# for better compression and faster loading. These rds files get saved in the "individual payment files" sub folder.
# This script also splits all the individual payment files up into year and state files to make it easier to
# retrieve data later on.
source("./data-raw/fsaFarmPayments/Supplementary Files/split_payment_files.R")
# Note should split the above script into three steps:
  # 1) convert excel files to RDS
  # 2) clean the individual RDS files
  # 3) split the RDS files into year-state files


# Need to create a script to handle all the geocoding.
#source("./data-raw/fsaFarmPayments/Supplementary Files/geocode_payment_files.R")

# Need to attach farm numbers using old WMB payment files and a fuzzy matching algorithm
#source("./data-raw/fsaFarmPayments/Supplementary Files/fuzzy_match_payment_files.R")

# Use CLUs to assign tract numbers to each observation using the farm numbers
# retrieved in the previous step.
#source("./data-raw/fsaFarmPayments/Supplementary Files/match_tract_numbers.R")


# Need to attach CDL to each payment to get crop numbers
# source(cdl script)


# Finally run aggregate_payment_files to combine each individual payment file to
# create county level data sets
source("./data-raw/fsaFarmPayments/Supplementary Files/aggregate_payment_files.R")


# Export county level file aggregated by payment year --------------------------
    fsaFarmPayments <- read_csv("./data-raw/fsaFarmPayments/Output/fsaFarmPayments_county_payment_year.csv")


    # rename columns
    fsaFarmPayments <- fsaFarmPayments %>% rename(
      `Payment Year` = payment_year,
       `FSA Program (Short)` = prog_abb,
       `FSA Program (Full)` = prog_full,
       FIPS = fips,
      `Disbursement Amount` = `Disbursement Amount`,
    )

    # make sure there are not duplicate entries
    fsaFarmPayments <- distinct(fsaFarmPayments)


    # clean up spending category column names
    colnames(fsaFarmPayments) <- gsub("Fsa","FSA",str_to_title(gsub("_"," ",colnames(fsaFarmPayments))))

    # convert the fsaFarmPayments data to a tibble before exporting
    fsaFarmPayments <- dplyr::as_tibble(fsaFarmPayments)

    # use the county level file in the package data folder
    usethis::use_data(fsaFarmPayments, overwrite = TRUE)


# Export county level file aggregated by program year---------------------------
    fsaFarmPaymentsProgYear <- read_csv("./data-raw/fsaFarmPayments/Output/fsaFarmPayments_county_program_year.csv")


    # rename columns
    fsaFarmPaymentsProgYear <- fsaFarmPaymentsProgYear %>% rename(
       `Program Year` = `Accounting Program Year`,
       `FSA Program (Short)` = prog_abb,
       `FSA Program (Full)` = prog_full,
       FIPS = fips,
       `Disbursement Amount` = `Disbursement Amount`
    )

    # make sure there are not duplicate entries
    fsaFarmPaymentsProgYear <- distinct(fsaFarmPaymentsProgYear)




    # clean up spending category column names
    colnames(fsaFarmPaymentsProgYear) <- gsub("Fsa","FSA",str_to_title(gsub("_"," ",colnames(fsaFarmPaymentsProgYear))))


    # convert the fsaFarmPaymentsProgYear data to a tibble before exporting
    fsaFarmPaymentsProgYear <- dplyr::as_tibble(fsaFarmPaymentsProgYear)


    # use the county level file in the package data folder
    usethis::use_data(fsaFarmPaymentsProgYear, overwrite = TRUE)


# Export county level file aggregated by fiscal year----------------------------
    fsaFarmPaymentsFiscYear <- read_csv("./data-raw/fsaFarmPayments/Output/fsaFarmPayments_county_fiscal_year.csv")


    # rename columns
    fsaFarmPaymentsFiscYear <- fsaFarmPaymentsFiscYear %>% rename(
       `Fiscal Year` = fiscal_year,
       `FSA Program (Short)` = prog_abb,
       `FSA Program (Full)` = prog_full,
       FIPS = fips,
       `Disbursement Amount` = `Disbursement Amount`
    )

    fsaFarmPaymentsFiscYear <- distinct(fsaFarmPaymentsFiscYear)


    # clean up spending category column names
    colnames(fsaFarmPaymentsFiscYear) <- gsub("Fsa","FSA",str_to_title(gsub("_"," ",colnames(fsaFarmPaymentsFiscYear))))


    # convert the fsaFarmPaymentsFiscYear data to a tibble before exporting
    fsaFarmPaymentsFiscYear <- dplyr::as_tibble(fsaFarmPaymentsFiscYear)


    # use the county level file in the package data folder
    usethis::use_data(fsaFarmPaymentsFiscYear, overwrite = TRUE)








