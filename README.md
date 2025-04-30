
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Installation

`rfsa` can be installed directly from GitHub using
`remotes::install_github("dylan-turner25/rfsa", force = TRUE)`.

# Usage

`rfsa` has no exported functions. To load an included data set use the
`data()` function with one of the names of the data sets listed in the
table below. For example, to load the `fsaMyaPrice` data set, use the
following code:

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

# Avaliable Datasets

The following table reports the currently available data sets in the
`rfsa` package. The `Last Updated` column indicates the last time the
data set was updated in the package. The `Data Download Date` indicates
the date that the raw input data for the current year was downloaded
from the FSA website (this is important as these data sets are sometimes
revised or contain projected values for in progress marketing years).

| Dataset | Description | Rows | Years | Last Updated | Data Download Date | Included Columns |
|:---|:---|---:|:---|:---|:---|:---|
| fsaArcCoBenchmarks | ARC-CO Benchmark Data | 163079 | 2014-2023 | 04/30/2025 | 04/30/2025 | fips, state_name, county_name, crop, unit, yield_type, program_year, benchmark_revenue, guarantee_revenue, maximum_payment_rate, actual_yield, national_price, actual_revenue, formula_payment_rate, payment_rate, oa_bench_mark_price, oa_bench_mark_yield, oa_bench_mark_years, arc_co_payment_rate, crop_type, rma_type_code, rma_crop_code |
| fsaArcCoPrice | ARC-CO benchmark prices. | 249 | 2014-2024 | 04/21/2025 | 04/10/2025 | Commodity, Marketing Year, Publishing Dates for the Final T-0 MYA Price and T-0 Actual ARC-CO Price, Unit, Statutory Reference Price, Final T-5 Annual Benchmark Price, Final T-4 Annual Benchmark Price, Final T-3 Annual Benchmark Price, Final T-2 Annual Benchmark Price, Final T-1 Annual Benchmark Price, Final T-0 ARC-CO Benchmark Price, Final T-0 MYA Price, T-0 National Loan Rate, Final T-0 Actual ARC-CO Price, year |
| fsaArcIcPrice | Commodity-specific ARC-IC benchmark prices, MYA prices, and statutory reference prices. | 227 | 2015-2024 | 04/19/2025 | 04/10/2025 | Commodity, Marketing Year, Publishing Dates for the Final T-0 MYA Price and T-0 ARC-IC BM Price, Unit, Statutory Reference Price, Final T-5 Annual Benchmark Price, Final T-4 Annual Benchmark Price, Final T-3 Annual Benchmark Price, Final T-2 Annual Benchmark Price, Final T-1 Annual Benchmark Price, Final T-0 MYA Price, T-0 National Loan Rate, Final T-0 Actual ARC-IC Price, year |
| fsaArcPlcBaseAcres | ARC/PLC enrolled base acres by commodity | 225 | 2015-2024 | 04/30/2025 | 04/10/2025 | covered_commodity, plc_covered_commodity_contract_base, plc_plantings_attributed_to_generic_base, arc_co_covered_commodity_contract_base, arc_co_plantings_attributed_to_generic_base, arc_ic_enrolled_base_covered_commodity_contract_base, total, plc_total, arc_co_total, arc_ic_total, year, arc_co_all, arc_co_irrigated, arc_co_nonirrigated |
| fsaArcPlcPayments | ARC/PLC Program Payments by Crop and Year | 481 | 2014-2023 | 04/22/2025 | 04/10/2025 | Program, Crop, year, Amount Paid |
| fsaEffectiveRefPrices | Effective Reference Prices for ARC/PLC Commodities | 158 | 2019-2025 | 04/30/2025 | 04/10/2025 | crop, marketing_year_dates, marketing_year, program_year, unit, statutory_reference_price, 115_statutory_reference_price, mya_price_lag5, mya_price_lag4, mya_price_lag3, mya_price_lag2, mya_price_lag1, 85_olympic_average_mya, effective_reference_price, crop_type, rma_type_code, rma_crop_code |
| fsaMyaPrice | FSA Marketing Year Average Prices | 245 | 2014-2024 | 04/30/2025 | 04/29/2025 | crop, marketing_year, marketing_year_dates, publishing_dates_for_final_mya_price, unit, current_mya_price, final_mya_price_lag1, final_mya_price_lag2, final_mya_price_lag3, final_mya_price_lag4, final_mya_price_lag5, final_mya_price_lag6, rma_crop_code, crop_type, rma_type_code |
| fsaPlcPaymentRate | Price Loss Coverage (PLC) Payment Rates by Crop and Program Year | 249 | 2014-2024 | 04/30/2025 | 04/10/2025 | crop, marketing_year_dates, marketing_year, program_year, publishing_dates_for_final_mya_price, statutory_reference_price, effective_reference_price, combined_reference_price, unit, current_mya_price, current_national_loan_rate, plc_price, plc_payment_rate, max_plc_payment_rate, crop_type, rma_type_code, rma_crop_code |
