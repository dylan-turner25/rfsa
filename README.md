rfsa: A package for accessing and analyzing USDA Farm Service Agency
data
================

- [Installation](#installation)
- [Usage](#usage)
- [ARC and PLC Program Data](#arc-and-plc-program-data)
- [FSA Individual Payment Files](#fsa-individual-payment-files)
- [Data Validation Checks](#data-validation-checks)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Installation

`rfsa` can be installed directly from GitHub using
`remotes::install_github("dylan-turner25/rfsa", force = TRUE)`.

# Usage

The`rfsa` package has no exported functions. To load an included data
set use the `data()` function with one of the names of the data sets
listed in the table below. For example, to load the `fsaMyaPrice` data
set, use the following code:

``` r
# load the rfsa package
library(rfsa)

# load marketing year average prices
data("fsaMyaPrice")
 
head(fsaMyaPrice)
#> # A tibble: 6 × 15
#>   crop          marketing_year marketing_year_dates publishing_dates_for…¹ unit 
#>   <chr>         <chr>          <chr>                <chr>                  <chr>
#> 1 wheat         2014-2015      Jun. 1-May 31        Jun. 29, 2015          Bush…
#> 2 barley        2014-2015      Jun. 1-May 31        Jun. 29, 2015          Bush…
#> 3 oats          2014-2015      Jun. 1-May 31        Jun. 29, 2015          Bush…
#> 4 peanuts       2014-2015      Aug. 1-Jul. 31       Aug. 31, 2015          Pound
#> 5 corn          2014-2015      Sep. 1-Aug. 31       Sep. 29, 2015          Bush…
#> 6 grain sorghum 2014-2015      Sep. 1-Aug. 31       Sep. 29, 2015          Bush…
#> # ℹ abbreviated name: ¹​publishing_dates_for_final_mya_price
#> # ℹ 10 more variables: current_mya_price <dbl>, final_mya_price_lag1 <dbl>,
#> #   final_mya_price_lag2 <dbl>, final_mya_price_lag3 <dbl>,
#> #   final_mya_price_lag4 <dbl>, final_mya_price_lag5 <dbl>,
#> #   final_mya_price_lag6 <dbl>, rma_crop_code <dbl>, crop_type <chr>,
#> #   rma_type_code <chr>
```

# ARC and PLC Program Data

The following table reports the currently available data sets related to
ARC and PLC in the `rfsa` package. The `Last Updated` column indicates
the last time the data set was updated in the package. The
`Data Download Date` indicates the date that the raw input data for the
current year was downloaded from the FSA website (this is important as
these data sets are sometimes revised or contain projected values for in
progress marketing years).

| Dataset | Description | Rows | Years | Last Updated | Data Download Date | Included Columns |
|:---|:---|---:|:---|:---|:---|:---|
| fsaArcCoBenchmarks | ARC-CO Benchmark Data | 163079 | 2014-2023 | 05/05/2025 | 05/05/2025 | fips, state_name, county_name, crop, unit, yield_type, program_year, benchmark_revenue, guarantee_revenue, maximum_payment_rate, actual_yield, national_price, actual_revenue, formula_payment_rate, payment_rate, oa_bench_mark_price, oa_bench_mark_yield, oa_bench_mark_years, arc_co_payment_rate, crop_type, rma_type_code, rma_crop_code |
| fsaArcCoPrice | ARC-CO Benchmark and Actual Price Data | 249 | 2014-2024 | 05/05/2025 | 05/05/2025 | crop, marketing_year_dates, publishing_dates_for_final_mya_price, unit, reference_price_combined, annual_benchmark_price_lag5, annual_benchmark_price_lag4, annual_benchmark_price_lag3, annual_benchmark_price_lag2, annual_benchmark_price_lag1, current_arcco_benchmark_price, current_mya_price, current_national_loan_rate, current_arcco_actual_price, marketing_year, program_year, crop_type, rma_type_code, rma_crop_code |
| fsaArcIcPrice | Commodity-specific ARC-IC benchmark prices, MYA prices, and statutory reference prices. | 227 | 2015-2024 | 05/05/2025 | 05/05/2025 | crop, marketing_year_dates, publishing_dates_for_final_mya_price, unit, reference_price_combined, annual_benchmark_price_lag5, annual_benchmark_price_lag4, annual_benchmark_price_lag3, annual_benchmark_price_lag2, annual_benchmark_price_lag1, current_mya_price, current_national_loan_rate, current_arcic_actual_price, marketing_year, program_year, crop_type, rma_type_code, rma_crop_code |
| fsaArcPlcBaseAcres | ARC/PLC enrolled base acres by commodity | 225 | 2015-2024 | 05/21/2025 | 05/05/2025 | covered_commodity, plc_covered_commodity_contract_base, plc_plantings_attributed_to_generic_base, arc_co_covered_commodity_contract_base, arc_co_plantings_attributed_to_generic_base, arc_ic_enrolled_base_covered_commodity_contract_base, total, plc_total, arc_co_total, arc_ic_total, program_year, arc_co_all, arc_co_irrigated, arc_co_nonirrigated, crop_type, rma_type_code, rma_crop_code, crop |
| fsaArcPlcPayments | ARC/PLC Program Payments by Crop and Year | 481 | 2014-2023 | 05/05/2025 | 05/05/2025 | program, crop, program_year, payments, crop_type, rma_type_code, rma_crop_code |
| fsaEffectiveRefPrices | Effective Reference Prices for ARC/PLC Commodities | 158 | 2019-2025 | 05/05/2025 | 05/05/2025 | crop, marketing_year_dates, marketing_year, program_year, unit, statutory_reference_price, 115_statutory_reference_price, mya_price_lag5, mya_price_lag4, mya_price_lag3, mya_price_lag2, mya_price_lag1, 85_olympic_average_mya, effective_reference_price, crop_type, rma_type_code, rma_crop_code |
| fsaMyaPrice | FSA Marketing Year Average Prices | 245 | 2014-2024 | 05/05/2025 | 05/05/2025 | crop, marketing_year, marketing_year_dates, publishing_dates_for_final_mya_price, unit, current_mya_price, final_mya_price_lag1, final_mya_price_lag2, final_mya_price_lag3, final_mya_price_lag4, final_mya_price_lag5, final_mya_price_lag6, rma_crop_code, crop_type, rma_type_code |
| fsaPlcPaymentRate | Price Loss Coverage (PLC) Payment Rates by Crop and Program Year | 249 | 2014-2024 | 05/05/2025 | 05/05/2025 | crop, marketing_year_dates, marketing_year, program_year, publishing_dates_for_final_mya_price, statutory_reference_price, effective_reference_price, combined_reference_price, unit, current_mya_price, current_national_loan_rate, plc_price, plc_payment_rate, max_plc_payment_rate, crop_type, rma_type_code, rma_crop_code |

# FSA Individual Payment Files

The USDA Farm Service Agency makes available Individual Payment Files
that contain payment information for programs administered by FSA.

# Data Validation Checks

The following table contains data validation checks. These are
comparisons between values derived from the `rfsa` package functions
against the same values obtained from another source. For example, the
first row calculates total ARC-CO payments in program year 2023 using
the `get_fsa_payments()` function and compares it to the total ARC-CO
payments in program year 2023 from an aggregated file on the FSA
website. The `check_passed` column indicates whether the difference
between the two values is less than 1%. If you are reading this and have
a value of interest that you would like to see added to this table,
please open an issue on the GitHub repository with the relevant
information including code to generate the value using the `rfsa`
package as well as an external source to validate the value against.

| value | code | package_value | external_value | external_source | percentage_difference | check_passed |
|:---|:---|---:|---:|:---|:---|:---|
| National ARC-CO payments in program year 2023 | get_fsa_payments(year = 2023,program = c(“ARC-CO”),year_type = “program”,aggregation = “national”)\[,“payment_amount”\] | 460388613 | 461724994 | <https://www.fsa.usda.gov/sites/default/files/2025-01/ARCCO%20Non-ProgYr%20Specific%20Payment%20Data%20%282025-01-06%29.xlsx> | %-0.2894 | <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: forestgreen !important;">✓</span> |
| National ARC-CO payments in program year 2023 | data(fsaArcPlcPayments); fsaArcPlcPayments %\>% filter(program == “ARC-CO”, program_year == 2023) %\>% group_by(program_year) %\>% summarize(payments = sum(payments)) | 461724994 | 461724994 | <https://www.fsa.usda.gov/sites/default/files/2025-01/ARCCO%20Non-ProgYr%20Specific%20Payment%20Data%20%282025-01-06%29.xlsx> | %0 | <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: forestgreen !important;">✓</span> |
