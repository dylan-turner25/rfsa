# Global variable declarations to satisfy R CMD check
# This file defines global variables used in the package to avoid NOTES during check

utils::globalVariables(c(
  # Dataset names
  "fsaMyaPrice",
  "fsaEffectiveRefPrices",
  "fsaPlcPaymentRate",
  "fsaPlcYields",
  "fsaArcCoBenchmarks",
  "fsaArcPlcData",
  "fsaUpdatedBaseAcres",

  # Column names from datasets
  "current_mya_price",
  "statutory_reference_price",
  "effective_reference_price",
  "current_national_loan_rate",
  "plc_yield",
  "oa_bench_mark_yield",
  "oa_bench_mark_price",
  "actual_yield",
  "yield_type",
  "state_name",
  "county_name",
  "program_year",
  "crop",
  "total_payment_value",
  "enrolled_base_PLC",
  "enrolled_base_ARCCO",
  "final_payment",
  "sequestration_rate",
  "scenario",
  "additional_base_acres",
  "base_acres",
  ".grp_enrolled",
  ".state_fips",
  ".state_arc_share",
  ".w",
  ".fallback_add",
  ".add_ARCCO",
  ".add_PLC",

  # dplyr/tidyverse variables
  ".data",
  ".env"
))