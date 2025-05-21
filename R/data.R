#' County‐level base acres and average PLC yields by crop
#'
#' A dataset containing county‐level base acreage and average PLC yield for each covered
#' commodity, along with associated RMA and FIPS codes, by program year.
#'
#' @format data frame with 99,308 rows and 12 variables:
#' \describe{
#'   \item{state}{character. Full name of the state.}
#'   \item{county}{character. Name of the county.}
#'   \item{state_code}{character. Two-digit state FIPS code.}
#'   \item{county_code}{character. Three-digit county FIPS code.}
#'   \item{crop}{character. Name of covered commodity}
#'   \item{crop_type}{character. Name of the crop type (e.g. "long grain", "temperate japonica").}
#'   \item{base_acres}{numeric. Base acreage for this crop in the county (acres).}
#'   \item{avg_plc_yield}{numeric. Average PLC yield for the crop.}
#'   \item{program_year}{integer. Program year (e.g. 2014-2023).}
#'   \item{rma_type_code}{character. RMA crop type code.}
#'   \item{rma_crop_code}{integer. RMA crop code.}
#'   \item{fips}{character. Five-digit combined state+county FIPS code.}
#' }
#'
#' @source \url{https://www.fsa.usda.gov/programs-and-services/arcplc_program/index}
#' @usage data(fsaCountyBaseAcres)
"fsaCountyBaseAcres"

#' Price Loss Coverage (PLC) Payment Rates by Crop and Program Year
#'
#' A dataset containing annual payment rate information for the Price Loss Coverage (PLC) program
#' administered by the USDA. This data includes statutory and effective reference prices,
#' marketing year average (MYA) prices, loan rates, and computed PLC payment rates by commodity
#' and program year.
#'
#' @format A data frame with 249 rows and 17 variables:
#' \describe{
#'   \item{crop}{Name of the commodity (e.g., wheat, corn, soybeans)}
#'   \item{marketing_year_dates}{Date range defining the marketing year (e.g., "Sep. 1–Aug. 31")}
#'   \item{marketing_year}{Formatted marketing year (e.g., "2014–2015")}
#'   \item{program_year}{FSA program year, typically defined as the year in which the crop is harvested.}
#'   \item{publishing_dates_for_final_mya_price}{Date when the final MYA price was released}
#'   \item{statutory_reference_price}{Reference price defined in statute}
#'   \item{effective_reference_price}{Calculated reference price including escalator provisions (if applicable)}
#'   \item{combined_reference_price}{The relevant reference price (i.e. statutory reference price until effective reference prices were introduced.)}
#'   \item{unit}{Measurement unit (e.g., "Bushel", "Pound")}
#'   \item{current_mya_price}{Final marketing year average price received by farmers}
#'   \item{current_national_loan_rate}{National loan rate for the commodity}
#'   \item{plc_price}{Price used in PLC payment calculations (typically the MYA)}
#'   \item{plc_payment_rate}{Final PLC payment rate per unit, equal to max(0, reference price - MYA)}
#'   \item{max_plc_payment_rate}{Maximum payment rate possible under PLC, defined as reference price - loan rate}
#'   \item{crop_type}{Subtype of crop where applicable (e.g., "large", "small", or grain class)}
#'   \item{rma_type_code}{Optional RMA crop insurance type code (if available)}
#'   \item{rma_crop_code}{Optional RMA crop insurance commodity code (if available)}
#' }
#'
#' @source \url{https://www.fsa.usda.gov/programs-and-services/arcplc_program/index}
#' @usage data(fsaPlcPaymentRate)
"fsaPlcPaymentRate"



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


#' Commodity-specific ARC-IC benchmark prices, MYA prices, and statutory reference prices.
#'
#' @details
#' To view code used to generate this data set, see `./data-raw/fsaArcPlc/supplementary_files/fsaArcIcPrice.R`
#'
#' @format A tibble
#' \describe{
#'   \item{crop}{Crop name or commodity covered by ARC/PLC.}
#'   \item{marketing_year_dates}{Date range defining the marketing year for the crop (e.g., "Sep. 1–Aug. 31").}
#'   \item{publishing_dates_for_final_mya_price}{Date the final MYA and ARC-IC actual prices were published.}
#'   \item{unit}{Unit of measure for the commodity (e.g., Bushel, Pound).}
#'   \item{reference_price_combined}{Statutory reference price up untill effective reference prices were introduced (following 2018 farm bill), at which point effective reference prices are reported}
#'   \item{annual_benchmark_price_lag5}{Price from five years prior to the marketing year (T-5), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag4}{Price from four years prior (T-4), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag3}{Price from three years prior (T-3), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag2}{Price from two years prior (T-2), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag1}{Price from one year prior (T-1), used in ARC-CO benchmark calculation.}
#'   \item{current_mya_price}{Marketing Year Average price for the current year (T-0).}
#'   \item{current_national_loan_rate}{National loan rate for the current marketing year.}
#'   \item{current_arcic_actual_price}{Actual ARC-IC price used in revenue calculations for T-0.}
#'   \item{marketing_year}{Marketing year (e.g., "2020–2021").}
#'   \item{program_year}{Program year corresponding to the marketing year (e.g., 2020).}
#'   \item{crop_type}{Subtype of the crop, if applicable (e.g., "long grain", "small").}
#'   \item{rma_type_code}{RMA type code used to classify crop type (may be NA).}
#'   \item{rma_crop_code}{RMA crop code used for identification of commodity.}
#' }
#' @usage data(fsaArcIcPrice)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
#'
"fsaArcIcPrice"


#' ARC-CO Benchmark Data
#'
#' A dataset containing ARC-CO benchmark and payment rate components, including yields, prices,
#' payment formulas, and benchmark calculations by commodity and county.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{actual_revenue}{Actual county-level revenue for the program year}
#'   \item{actual_yield}{County yield for the program year}
#'   \item{arc_co_payment_rate}{Final ARC-CO payment rate per unit}
#'   \item{benchmark_revenue}{Benchmark revenue calculated using Olympic average yields and prices}
#'   \item{county_name}{Name of the county}
#'   \item{crop}{Name of the commodity}
#'   \item{crop_type}{Crop subtype if applicable (e.g., "long grain")}
#'   \item{fips}{5-digit FIPS code for county identification}
#'   \item{formula_payment_rate}{The formula-derived payment rate prior to maximum cap}
#'   \item{guarantee_revenue}{88% of the benchmark revenue}
#'   \item{maximum_payment_rate}{Maximum allowable payment rate (10% of benchmark revenue)}
#'   \item{national_price}{Final national price used in revenue calculations}
#'   \item{oa_bench_mark_price}{Olympic average benchmark price}
#'   \item{oa_bench_mark_years}{Marketing years used for the Olympic average}
#'   \item{oa_bench_mark_yield}{Olympic average county yield}
#'   \item{payment_rate}{The final payment rate after applying caps}
#'   \item{program_year}{Crop year corresponding to payment eligibility}
#'   \item{rma_crop_code}{RMA commodity code}
#'   \item{rma_type_code}{RMA type classification code}
#'   \item{state_name}{Name of the state}
#'   \item{unit}{Unit of measurement (e.g., bushel, pound)}
#'   \item{yield_type}{Type of yield used in benchmark (e.g., NASS or RMA)}
#' }
#'
#' @usage data(fsaArcCoBenchmarks)
#' @source \url{https://www.fsa.usda.gov/programs-and-services/arcplc_program}
"fsaArcCoBenchmarks"


#' ARC-CO Benchmark and Actual Price Data
#'
#' A dataset containing annual commodity-level price data used in the calculation of ARC-CO (Agriculture Risk Coverage - County) benchmark and actual prices, including statutory and market-based price indicators.
#'
#' @format A tibble with one row per crop, marketing year, and crop type (if applicable). Variables include:
#' \describe{
#'   \item{crop}{Crop name or commodity covered by ARC/PLC.}
#'   \item{marketing_year_dates}{Date range defining the marketing year for the crop (e.g., "Sep. 1–Aug. 31").}
#'   \item{publishing_dates_for_final_mya_price}{Date the final MYA and ARC-CO actual prices were published.}
#'   \item{unit}{Unit of measure for the commodity (e.g., Bushel, Pound).}
#'   \item{reference_price_combined}{Statutory reference price up untill effective reference prices were introduced (following 2018 farm bill), at which point effective reference prices are reported}
#'   \item{annual_benchmark_price_lag5}{Price from five years prior to the marketing year (T-5), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag4}{Price from four years prior (T-4), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag3}{Price from three years prior (T-3), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag2}{Price from two years prior (T-2), used in ARC-CO benchmark calculation.}
#'   \item{annual_benchmark_price_lag1}{Price from one year prior (T-1), used in ARC-CO benchmark calculation.}
#'   \item{current_arcco_benchmark_price}{ARC-CO benchmark price for the current marketing year (T-0).}
#'   \item{current_mya_price}{Marketing Year Average price for the current year (T-0).}
#'   \item{current_national_loan_rate}{National loan rate for the current marketing year.}
#'   \item{current_arcco_actual_price}{Actual ARC-CO price used in revenue calculations for T-0.}
#'   \item{marketing_year}{Marketing year (e.g., "2020–2021").}
#'   \item{program_year}{Program year corresponding to the marketing year (e.g., 2020).}
#'   \item{crop_type}{Subtype of the crop, if applicable (e.g., "long grain", "small").}
#'   \item{rma_type_code}{RMA type code used to classify crop type (may be NA).}
#'   \item{rma_crop_code}{RMA crop code used for identification of commodity.}
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
#'   \item{program}{Character. Type of commodity program: \code{"ARC-CO"}, \code{"ARC-IC"}, or \code{"PLC"}.}
#'   \item{crop}{Character. Name of the crop associated with the payment (e.g., \code{"Corn"}, \code{"Soybeans"}, \code{"Wheat"}).}
#'   \item{program_year}{Integer. Program year associated with the payment. Program years are defined by FSA as the year the commodity was harvested. Program year does not typically reflect the calendar year when the payment is disbursed. I.e. for many crops harvested in the fall, the payment will be recieved by the producer in the next calendar year.}
#'   \item{payments}{Numeric. Total amount paid for the given crop, year, and program (in nominal dollars).}
#'   \item{crop_type}{Subtype of the crop, if applicable (e.g., "long grain", "small").}
#'   \item{rma_type_code}{RMA type code used to classify crop type (may be NA).}
#'   \item{rma_crop_code}{RMA crop code used for identification of commodity.}
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
#' statutory reference prices, 115% caps, lagged Marketing Year Average (MYA) prices, Olympic averages,
#' and the final effective reference price used to determine program payments. The dataset also includes
#' metadata on crop classification and program year alignment.
#'
#' @format A data frame with 17 variables:
#' \describe{
#'   \item{crop}{Name of the commodity (e.g., Corn, Wheat, Peanuts)}
#'   \item{marketing_year_dates}{The official marketing year for the commodity (e.g., "Sep. 1–Aug. 31")}
#'   \item{marketing_year}{Label for the marketing year period (e.g., "2019-2020")}
#'   \item{program_year}{The crop year used for program payment eligibility (e.g., 2019). FSA typically defines the program year as the calendar year in which the commodity is harvested.}
#'   \item{unit}{Unit of measurement (e.g., Bushel, Pound)}
#'   \item{statutory_reference_price}{The statutory reference price for the crop}
#'   \item{115_statutory_reference_price}{The statutory reference price multiplied by 115%, used as a cap for the effective reference price}
#'   \item{mya_price_lag5}{Marketing Year Average (MYA) price from five years prior}
#'   \item{mya_price_lag4}{MYA price from four years prior}
#'   \item{mya_price_lag3}{MYA price from three years prior}
#'   \item{mya_price_lag2}{MYA price from two years prior}
#'   \item{mya_price_lag1}{MYA price from one year prior}
#'   \item{85_olympic_average_mya}{85% of the Olympic average of the five MYA prices, dropping the high and low}
#'   \item{effective_reference_price}{Final effective reference price used for the program year}
#'   \item{crop_type}{Subtype of the crop, if applicable (e.g., "long grain", "large")}
#'   \item{rma_type_code}{RMA classification code for crop type, if available}
#'   \item{rma_crop_code}{RMA crop code used for cross-agency alignment}
#' }
#'
#' @usage data(fsaEffectiveRefPrices)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
"fsaEffectiveRefPrices"

#' ARC/PLC enrolled base acres by commodity
#'
#' This dataset provides annual base acreage by program and commodity under the ARC-CO,
#' ARC-IC, and PLC programs administered by the USDA Farm Service Agency. It includes
#' acreage originally attributed to generic base, as well as total enrolled acreage by commodity
#' and year. From 2018 onward, base assignments under the Seed Cotton program are also reflected.
#'
#' @format A data frame with 225 rows and 18 variables:
#' \describe{
#'   \item{covered_commodity}{character. FSA’s covered commodity identifier (e.g. "rice-temp japonica").}
#'   \item{plc_covered_commodity_contract_base}{numeric. Acres enrolled in PLC with covered-commodity base.}
#'   \item{plc_plantings_attributed_to_generic_base}{numeric. Acres assigned to PLC from generic base.}
#'   \item{arc_co_covered_commodity_contract_base}{numeric. Acres enrolled in ARC-CO with covered-commodity base.}
#'   \item{arc_co_plantings_attributed_to_generic_base}{numeric. Acres assigned to ARC-CO from generic base.}
#'   \item{arc_ic_enrolled_base_covered_commodity_contract_base}{numeric. Acres enrolled in ARC-IC with covered-commodity base.}
#'   \item{total}{numeric. Total base acres enrolled for the commodity across all programs.}
#'   \item{plc_total}{numeric. Total PLC base acres (sum across all commodities).}
#'   \item{arc_co_total}{numeric. Total ARC-CO base acres (sum across all commodities).}
#'   \item{arc_ic_total}{numeric. Total ARC-IC base acres (sum across all commodities).}
#'   \item{program_year}{integer. Program year (e.g. 2018, 2019, …).}
#'   \item{arc_co_all}{numeric. ARC-CO acreage (all base types), available from 2018 onward.}
#'   \item{arc_co_irrigated}{numeric. Irrigated ARC-CO acreage (acres), available from 2018 onward.}
#'   \item{arc_co_nonirrigated}{numeric. Non-irrigated ARC-CO acreage (acres), available from 2018 onward.}
#'   \item{crop_type}{character. FSA crop-type classification (e.g. "long grain", "temperate japonica").}
#'   \item{rma_type_code}{character. RMA program type code.}
#'   \item{rma_crop_code}{integer. RMA crop code.}
#'   \item{crop}{character. Duplicate of covered_commodity for convienence when other data sets reference "crop"}
#' }
#'
#' @usage data(fsaArcPlcBaseAcres)
#' @source \url{https://www.fsa.usda.gov/resources/programs/arc-plc/program-data}
"fsaArcPlcBaseAcres"




