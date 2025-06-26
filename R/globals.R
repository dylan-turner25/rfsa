# Global variable declarations to satisfy R CMD check
# This file defines global variables used in the package to avoid NOTES during check

utils::globalVariables(c(
  # Dataset names
  "fsaMyaPrice",
  "fsaEffectiveRefPrices", 
  "fsaPlcPaymentRate",
  "fsaPlcYields",
  
  # Column names from datasets
  "current_mya_price",
  "statutory_reference_price",
  "effective_reference_price", 
  "current_national_loan_rate",
  "plc_yield",
  
  # dplyr/tidyverse variables
  ".data",
  ".env"
))