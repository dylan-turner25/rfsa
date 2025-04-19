# load necessary libraries
library(tidyverse)

# define current year
current_year <- as.numeric(substr(Sys.Date(),1,4))

# define range of years to download data for
years <- 2014:current_year



# clean MYA Prices -------------------------------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaMyaPrice.R")

# clean PLC payment rates ------------------------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaPlcPaymentRates.R")

# clean ARC-IC Prices ----------------------------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaArcIcPrice.R")

# clean ARC-CO Prices ----------------------------------------------------------
source("./data-raw/fsaArcPlc/Supplementary Files/fsaArcCoPrice.R")  # updated 2/03/2025

# clean base acres -------------------------------------------------------------
# https://farmdocdaily.illinois.edu/2021/09/2021-commodity-program-enrollment-dashboard.html

# clean arc-plc payments
source("./data-raw/fsaArcPlc/Supplementary Files/fsaArcPlcPayments_V02.R")   # updated 2/03/2025

# clean effective reference prices
source("./data-raw/fsaArcPlc/Suplementary Files/fsaEffectiveRefPrices.R") # updated 2/03/2025

# clean arc/plc base acres
source("./data-raw/fsaArcPlc/Suplementary Files/fsaArcPlcBaseAcres.R") # updated 2/03/2025


# clean arc/plc elections ------------------------------------------
# source("./data-raw/fsaArcPlc/Supplementary Files/fsaArcPlcElections.R")

# # clean arc benchmarks ---------------------------------------------
# source("./data-raw/fsaArcPlc/Supplementary Files/fsaArcCoBenchmarks.R")

# # clean plc benchmarks ---------------------------------------------
# source("./data-raw/fsaArcPlc/Supplementary Files/fsaPlcGuarantee.R")





