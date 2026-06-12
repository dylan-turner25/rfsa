# load necessary libraries
library(tidyverse)

# define current year
current_year <- as.numeric(substr(Sys.Date(),1,4))

# define range of years to download data for
years <- 2014:current_year

# Note: for up-to-date arc/plc projections, fsaMyaPrice.R and fsaArcCoBenchmarks.R
# are the most frequently updated and critical files that get updated monthly.
#The other data are generally not updated frequently.Those two files also feed into fsaArcPlcData.R

# full arc plc data used for arc/plc projections
source("./data-raw/fsaArcPlc/supplementary_files/fsaArcPlcData.R")

# plc yields ----------------------------------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaPlcYields.R")

# clean county level total base acres ---------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaCountyBaseAcres.R")

# clean county level arc plc enrollment data --------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaEnrolledCountyBaseAcres.R")

# clean MYA Prices -------------------------------------------------------------
current_year = 2026
source("./data-raw/fsaArcPlc/supplementary_files/fsaMyaPrice.R")

# clean PLC payment rates ------------------------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaPlcPaymentRates.R")

# clean ARC-IC Prices ----------------------------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaArcIcPrice.R")

# clean ARC-CO Prices ----------------------------------------------------------
source("./data-raw/fsaArcPlc/supplementary_files/fsaArcCoPrice.R")

# clean base acres -------------------------------------------------------------
# https://farmdocdaily.illinois.edu/2021/09/2021-commodity-program-enrollment-dashboard.html

# clean arc-plc payments
source("./data-raw/fsaArcPlc/Supplementary Files/fsaArcPlcPayments.R")

# clean effective reference prices
source("./data-raw/fsaArcPlc/supplementary_files/fsaEffectiveRefPrices.R")

# clean arc/plc base acres
source("./data-raw/fsaArcPlc/Suplementary Files/fsaArcPlcBaseAcres.R") # updated 2/03/2025


# clean arc/plc elections ------------------------------------------
# source("./data-raw/fsaArcPlc/Supplementary Files/fsaArcPlcElections.R")

# clean arc benchmarks ---------------------------------------------
current_year = 2026
source("./data-raw/fsaArcPlc/supplementary_files/fsaArcCoBenchmarks.R")
# # clean plc benchmarks ---------------------------------------------
# source("./data-raw/fsaArcPlc/Supplementary Files/fsaPlcGuarantee.R")





