#' FSA Marketing Year Average Prices
#'
#' A dataset containing USDA FSA marketing-year average (MYA) prices for a range of commodities,
#' including the current MYA price and the six most recent lagged MYA prices, as well as RMA
#' crop codes and type classifications.
#'
#' @format A tibble with 245 rows and 15 columns:
#' \describe{
#'   \item{crop}{\code{character}. Commodity name (e.g., “wheat”, “corn”).}
#'   \item{marketing_year}{\code{character}. Marketing year label spanning two calendar years (e.g., “2014-2015”).}
#'   \item{marketing_year_dates}{\code{character}. Date range of the marketing year (e.g., “Jun. 1–May 31”).}
#'   \item{publishing_dates_for_final_mya_price}{\code{character}. Date the final T-0 MYA price was published (e.g., “Jun. 29, 2015”).}
#'   \item{unit}{\code{character}. Unit of measurement for the price (e.g., “Bushel”, “Pound”).}
#'   \item{current_mya_price}{\code{numeric}. Final T-0 MYA price for the marketing year defined by \code{marketing_year}.}
#'   \item{final_mya_price_lag1}{\code{numeric}. Final T-1 MYA price (one year prior).}
#'   \item{final_mya_price_lag2}{\code{numeric}. Final T-2 MYA price (two years prior).}
#'   \item{final_mya_price_lag3}{\code{numeric}. Final T-3 MYA price (three years prior).}
#'   \item{final_mya_price_lag4}{\code{numeric}. Final T-4 MYA price (four years prior).}
#'   \item{final_mya_price_lag5}{\code{numeric}. Final T-5 MYA price (five years prior).}
#'   \item{final_mya_price_lag6}{\code{numeric}. Final T-6 MYA price (six years prior).}
#'   \item{rma_crop_code}{\code{integer}. RMA commodity code identifying the insured crop (e.g., 41 = corn).}
#'   \item{crop_type}{\code{character}. Crop type classification (e.g., “long grain”, “seed”, “large”, “small”).}
#'   \item{rma_type_code}{\code{character}. Sub-type code used by RMA (e.g., “453” for long-grain rice).}
#' }
#'
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
#' @usage data(fsaMyaPrice)
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

#' ARC-CO benchmark prices.
#'
#' A dataset containing annual commodity-level price data used in the calculation of ARC-CO (Agriculture Risk Coverage - County) benchmark prices.
#'
#' @format A tibble
#' \describe{
#'   \item{Commodity}{Crop name or commodity covered by ARC-CO and MYA prices.}
#'   \item{Marketing Year}{The dates that define the commodity specific marketing year (e.x. Sep. 1-Aug. 31).}
#'   \item{Publishing Dates for the Final T-0 MYA Price and T-0 Actual ARC-CO Price}{Date the final prices for the marketing year were published.}
#'   \item{Unit}{Unit of measure for the commodity (e.g., Bushel, Pound).}
#'   \item{Statutory Reference Price}{Reference price set in legislation, used in ARC and PLC payment calculations.}
#'   \item{Final T-5 Annual Benchmark Price}{Price from five years prior to the marketing year, used in ARC-CO benchmark calculation.}
#'   \item{Final T-4 Annual Benchmark Price}{Price from four years prior to the marketing year, used in ARC-CO benchmark calculation.}
#'   \item{Final T-3 Annual Benchmark Price}{Price from three years prior to the marketing year, used in ARC-CO benchmark calculation.}
#'   \item{Final T-2 Annual Benchmark Price}{Price from two years prior to the marketing year, used in ARC-CO benchmark calculation.}
#'   \item{Final T-1 Annual Benchmark Price}{Price from one year prior to the marketing year, used in ARC-CO benchmark calculation.}
#'   \item{Final T-0 ARC-CO Benchmark Price}{ARC-CO benchmark price for the current marketing year.}
#'   \item{Final T-0 MYA Price}{Marketing Year Average price for the current year.}
#'   \item{T-0 National Loan Rate}{National loan rate for the current marketing year.}
#'   \item{Final T-0 Actual ARC-CO Price}{Actual price used in ARC-CO calculations}
#'   \item{year}{Marketing year (e.g., "2020-2021").}
#' }
#'
#' @usage data(fsaArcCoPrice)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
"fsaArcCoPrice"

#' ARC/PLC Program Payments by Crop and Year
#'
#' This dataset provides annual payments made through the ARC-CO, ARC-IC, and PLC programs
#' administered by the Farm Service Agency (FSA). It includes crop-level payment data across
#' multiple program years and crops, expressed in nominal dollars.
#'
#' @format A data frame with 481 rows and 4 variables:
#' \describe{
#'   \item{Program}{Character. Type of commodity program: \code{"ARC-CO"}, \code{"ARC-IC"}, or \code{"PLC"}.}
#'   \item{Crop}{Character. Name of the crop associated with the payment (e.g., \code{"Corn"}, \code{"Soybeans"}, \code{"Wheat"}).}
#'   \item{year}{Integer. Marketing year associated with the payment (e.g., \code{2014}, \code{2015}).}
#'   \item{Amount Paid}{Numeric. Total amount paid for the given crop, year, and program (in nominal dollars).}
#' }
#'
#'
#' @usage data(fsaArcCoPrice)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
"fsaArcPlcPayments"


#' Effective Reference Prices for ARC/PLC Commodities
#'
#' A dataset containing statutory and effective reference price calculations for commodities eligible
#' for the Price Loss Coverage (PLC) and Agricultural Risk Coverage (ARC) programs. This includes
#' historical Marketing Year Average (MYA) prices, statutory reference prices, calculated Olympic averages,
#' and the final effective reference price used to determine program payments.
#'
#' @format A data frame with 15 variables:
#' \describe{
#'   \item{Commodity}{Name of the commodity (e.g., Corn, Wheat, Peanuts)}
#'   \item{Marketing Year}{The official marketing year calendar for the commodity (e.g., "Sep. 1–Aug. 31")}
#'   \item{Unit}{Unit of measurement (e.g., Bushel, Pound)}
#'   \item{T-0 Reference Price}{The statutory reference price for the current marketing year}
#'   \item{115% of T-0 Reference Price}{115% of the statutory reference price, used as a ceiling for the effective reference price}
#'   \item{T-5  MYA Price}{Marketing Year Average (MYA) price from five years prior}
#'   \item{T-4  MYA Price}{MYA price from four years prior}
#'   \item{T-3 MYA Price}{MYA price from three years prior}
#'   \item{T-2 MYA Price}{MYA price from two years prior}
#'   \item{T-1 MYA Price}{MYA price from one year prior}
#'   \item{85% of 5-year avg, dropping high and low prices}{Calculated 85% of the Olympic average of the five MYA prices}
#'   \item{T-0 Effective Reference Price}{Final effective reference price for the marketing year}
#'   \item{MAX}{Maximum value among the five MYA prices}
#'   \item{MIN}{Minimum value among the five MYA prices}
#'   \item{year}{Program year (e.g., "2021-2022")}
#' }
#'
#' @usage data(fsaEffectiveRefPrices)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
"fsaEffectiveRefPrices"


#' ARC/PLC enrolled base acres by commodity
#'
#' This dataset provides annual  base acreage by program and commodity under the ARC-CO, ARC-IC, and PLC programs
#' administered by the USDA Farm Service Agency. It includes acreage originally attributed to generic base, as well as total
#' enrolled acreage by commodity and year. From 2018 onward, base assignments under the Seed Cotton program are also reflected.
#'
#' @format A data frame with 225 rows and 14 variables:
#' \describe{
#'   \item{covered_commodity}{Name of the covered commodity}
#'   \item{plc_covered_commodity_contract_base}{Acres enrolled in PLC with covered commodity base}
#'   \item{plc_plantings_attributed_to_generic_base}{Acres assigned to PLC from generic base}
#'   \item{arc_co_covered_commodity_contract_base}{Acres enrolled in ARC-CO with covered commodity base}
#'   \item{arc_co_plantings_attributed_to_generic_base}{Acres assigned to ARC-CO from generic base}
#'   \item{arc_ic_enrolled_base_covered_commodity_contract_base}{Acres enrolled in ARC-IC with covered commodity base}
#'   \item{total}{Total base acres enrolled for the commodity in all programs}
#'   \item{plc_total}{Total PLC base acres}
#'   \item{arc_co_total}{Total ARC-CO base acres}
#'   \item{arc_ic_total}{Total ARC-IC base acres}
#'   \item{year}{Year of enrollment}
#'   \item{arc_co_all}{ARC-CO acreage (all base types), available for 2018 onward}
#'   \item{arc_co_irrigated}{Irrigated ARC-CO acreage, available for 2018 onward}
#'   \item{arc_co_nonirrigated}{Nonirrigated ARC-CO acreage, available for 2018 onward}
#' }
#'
#' @usage data(fsaArcPlcBaseAcres)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
"fsaArcPlcBaseAcres"




