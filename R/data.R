#' Commodity specific marketing year average prices.
#'
#' @details
#' To view code used to generate this data set, see `./data-raw/fsaArcPlc/supplementary_files/fsaMyaPrice.R`
#'
#'
#' @format  A tibble
#' \describe{
#'   \item{Commodity}{The name of the commodity}
#'   \item{Marketing Year}{The dates for the commodity specific marketing year }
#'   \item{year}{The two calendar years that the commodity specific marketing year spans}
#'   \item{Publishing Dates for the  Final T - 0 MYA Price}{The date the final T-0 MYA price was published}
#'   \item{Unit}{The unit of measurement for the price}
#'   \item{Final T - 6 MYA Price}{The marketing year average price for the marketing year 6 years prior to the marketing year defined by `year`}
#'   \item{Final T - 5 MYA Price}{The marketing year average price for the marketing year 5 years prior to the marketing year defined by `year`}
#'   \item{Final T - 4 MYA Price}{The marketing year average price for the marketing year 4 years prior to the marketing year defined by `year`}
#'   \item{Final T - 3 MYA Price}{The marketing year average price for the marketing year 3 years prior to the marketing year defined by `year`}
#'   \item{Final T - 2 MYA Price}{The marketing year average price for the marketing year 2 years prior to the marketing year defined by `year`}
#'   \item{Final T - 1 MYA Price}{The marketing year average price for the marketing year 1 year prior to the marketing year defined by `year`}
#'   \item{Final T - 0 MYA Price}{The marketing year average price for the marketing year defined by `year`}
#'   }
#' @usage data(fsaMyaPrice)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
#'
"fsaMyaPrice"

#' Commodity specific PLC payment rates including statutory and effective reference prices.
#'
#' @details
#' To view code used to generate this data set, see `./data-raw/fsaArcPlc/supplementary_files/fsaPlcPaymentRates.R`
#'
#'
#' @format  A tibble
#' \describe{
#'   \item{Commodity}{The name of the commodity}
#'   \item{Marketing Year}{The dates for the commodity specific marketing year }
#'   \item{year}{The two calendar years that the commodity specific marketing year spans}
#'   \item{Publishing Dates for the  Final T - 0 PLC Effective Price}{The date the final T-0 PLC effective price was published}
#'   \item{Unit}{The unit of measurement for the price}
#'   \item{Statutory Reference Price}{The statutory reference price for the commodity}
#'   \item{Final T-0 Effective Price}{The effective reference price for the commodity in the marketing year defined by `year`}
#'   \item{Final T-0 MYA Price}{The marketing year average price for the marketing year defined by `year`}
#'   \item{T-0 National Loan Rate}{The national loan rate for the commodity in the marketing year defined by `year`}
#'   \item{Final T-0 PLC Payment Rate}{The PLC payment rate for the commodity in the marketing year defined by `year`}
#'   \item{Maximum PLC Payment Rate}{The maximum PLC payment rate for the commodity in the marketing year defined by `year`}
#'   }
#' @usage data(fsaPlcPaymentRate)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
#'
"fsaPlcPaymentRate"


#' Commodity-specific ARC-IC benchmark prices, MYA prices, and statutory reference prices.
#'
#' @details
#' To view code used to generate this data set, see `./data-raw/fsaArcPlc/supplementary_files/fsaArcIcPrice.R`
#'
#' @format A tibble
#' \describe{
#'   \item{Commodity}{The name of the commodity}
#'   \item{Marketing Year}{The dates for the commodity-specific marketing year}
#'   \item{Publishing Dates for the Final T - 0 MYA Price and T - 0 ARC-IC BM Price}{The publication date of the final marketing year average price and ARC-IC benchmark price}
#'   \item{Unit}{The unit of measurement (e.g., bushel, pound)}
#'   \item{Statutory Reference Price}{The statutory reference price for the commodity}
#'   \item{Final T - 5 Annual Benchmark Price}{The ARC-IC benchmark price 5 years prior to the marketing year}
#'   \item{Final T - 4 Annual Benchmark Price}{The ARC-IC benchmark price 4 years prior to the marketing year}
#'   \item{Final T - 3 Annual Benchmark Price}{The ARC-IC benchmark price 3 years prior to the marketing year}
#'   \item{Final T - 2 Annual Benchmark Price}{The ARC-IC benchmark price 2 years prior to the marketing year}
#'   \item{Final T - 1 Annual Benchmark Price}{The ARC-IC benchmark price 1 year prior to the marketing year}
#'   \item{Final T - 0 MYA Price}{The final marketing year average price}
#'   \item{T - 0 National Loan Rate}{The national loan rate for the marketing year}
#'   \item{Final T - 0 Actual ARC-IC Price}{The actual ARC-IC price for the marketing year}
#'   \item{year}{The two calendar years the commodity-specific marketing year spans}
#' }
#' @usage data(fsaArcIcPrice)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
#'
"fsaArcIcPrice"


#' ARC-CO benchmark revenues by county and crop
#'
#' This dataset contains county-level benchmark and actual revenue data related to the USDA's Agriculture Risk Coverage – County (ARC-CO) program.
#' The data spans multiple years and includes information on benchmark yields, benchmark prices, actual yields, and ARC-CO payment rates.
#'
#' @format A data frame with 21 variables:
#' \describe{
#'   \item{fips}{5-digit FIPS code identifying the county.}
#'   \item{state_name}{Name of the state.}
#'   \item{county_name}{Name of the county.}
#'   \item{crop_name}{Name of the crop (e.g., Barley).}
#'   \item{unit}{Unit of measurement (e.g., Bushel).}
#'   \item{yield_type}{Yield type (e.g., All, Irrigated, Non-Irrigated).}
#'   \item{year}{Year of the observation.}
#'   \item{county_yield}{County yield for the year (used in construction of the benchmark revenue).}
#'   \item{benchmark_revenue}{Benchmark revenue for the county and crop.}
#'   \item{guarantee_revenue}{ARC-CO guaranteed revenue level.}
#'   \item{maximum_payment_rate}{Maximum possible payment rate under ARC-CO.}
#'   \item{actual_yield}{Actual yield in the county for the crop and year.}
#'   \item{national_price}{National average price for the crop.}
#'   \item{actual_revenue}{Actual revenue in the county. Calculated using actual yield and price.}
#'   \item{formula_payment_rate}{Formula-based payment rate prior to limits.}
#'   \item{payment_rate}{Final ARC-CO payment rate.}
#'   \item{oa_bench_mark_price}{Olympic average benchmark price.}
#'   \item{oa_bench_mark_yield}{Olympic average benchmark yield.}
#'   \item{oa_bench_mark_years}{Years used in calculating the Olympic averages.}
#'   \item{county_yield_type}{Description of the county yield methodology.}
#'   \item{arc_co_payment_rate}{ARC-CO payment rate field (may be duplicated or alternative version).}
#' }
#'
#' @usage data(fsaArcCoBenchmarks)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
"fsaArcCoBenchmarks"
