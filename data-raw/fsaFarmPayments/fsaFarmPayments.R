
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

# Third, run this file to apply some cleaning operations, then convert each individual payment file to rds files
# for better compression and faster loading.
# This script also splits all the individual payment files up into year and state files to make it easier to
# retrieve data later on.
source("./data-raw/fsaFarmPayments/supplementary_files/split_payment_files.R")
# Note should probably split the above script into three steps:
  # 1) convert excel files to RDS
  # 2) clean the individual RDS files
  # 3) split the RDS files into year/program files




