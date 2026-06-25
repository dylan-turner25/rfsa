
# load libraries
library(tidyverse)
library(readxl)
library(data.table)


# First run download_payment_files.R to download each of the individual farm payment excel files from the FSA website.
# Note: I abandoned the idea of an automated downloading script. It was becoming too inefficient to
# maintain it and not worth the effort given that the marginal cost of downloading the new year's
# files manually is about 5 minutes.
#source("./data-raw/fsaFarmPayments/supplementary_files/download_payment_files.R")

# Second, run get_program_descriptions.R to create an excel file of unique program descriptions.
# This will add any new program descriptions into an csv file "program_details.csv". If there
# are new program descriptions, and error will be issued that prompts the user to open that
# csv file any fill out any missing information before continuing.
source("./data-raw/fsaFarmPayments/supplementary_files/get_program_descriptions.R")


# run this script to convert each excel file into an rds file before saving. This
# cuts load times way down if you have to go back to the source files to rebuild from scratch
source("./data-raw/fsaFarmPayments/supplementary_files/convert_to_rds.R")


# run this script to apply some cleaning operations to each rds file from the previous step.
# these files all receive a "cleaned" suffix in the file name.
source("./data-raw/fsaFarmPayments/supplementary_files/clean_rds.R")


# This script also splits all the individual payment files up into year and state files to make it easier to
# retrieve data later on. These year-program files get saved in the inst/extdata/fsaFarmPayments folder and
# are what is accessed within the package.
source("./data-raw/fsaFarmPayments/supplementary_files/split_payment_files.R")



# Delete empty RDS files (309B, 0 observations) before upload
empty_files <- list.files("data-raw/fsaFarmPayments/output_data", "\\.rds$",
                         full.names = TRUE, recursive = TRUE)
empty_files <- empty_files[file.size(empty_files) == 309]

if(length(empty_files) > 0) {
  cat("Deleting", length(empty_files), "empty RDS files (309B)...\n")
  file.remove(empty_files)
  cat("Empty files deleted successfully.\n")
} else {
  cat("No empty files found.\n")
}

# Create three separate releases for the different file types
piggyback::pb_new_release(
  repo = "dylan-turner25/rfsa",
  tag  = "fiscal",
  name = "Fiscal Year Files",
  body = "This release contains all fiscal year files."
)

piggyback::pb_new_release(
  repo = "dylan-turner25/rfsa",
  tag  = "payment",
  name = "Payment Year Files",
  body = "This release contains all payment year files."
)

piggyback::pb_new_release(
  repo = "dylan-turner25/rfsa",
  tag  = "program",
  name = "Program Year Files",
  body = "This release contains all program year files."
)

# Get file lists for each category
fiscal_files <- list.files("data-raw/fsaFarmPayments/output_data/fiscal_year_files", "\\.rds$",
                          full.names = TRUE, recursive = TRUE)

payment_files <- list.files("data-raw/fsaFarmPayments/output_data/payment_year_files", "\\.rds$",
                           full.names = TRUE, recursive = TRUE)

program_files <- list.files("data-raw/fsaFarmPayments/output_data/program_year_files", "\\.rds$",
                           full.names = TRUE, recursive = TRUE)



options(piggyback.verbose = T)

# Upload fiscal year files
piggyback::pb_upload(
  fiscal_files,
  repo = "dylan-turner25/rfsa",
  tag  = "fiscal",
  overwrite = F
)

# Upload payment year files
piggyback::pb_upload(
  payment_files,
  repo = "dylan-turner25/rfsa",
  tag  = "payment",
  overwrite = F
)

# Upload program year files
piggyback::pb_upload(
  program_files,
  repo = "dylan-turner25/rfsa",
  tag  = "program",
  overwrite = F
)



