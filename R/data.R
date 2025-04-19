#' Commodity specific marketing year average prices as published by the USDA Farm Service Agency.
#'
#' @description Marketing year average prices by commodity.
#'
#' @details
#' To view code used to generate this data set, see `./data-raw/fsaArcPlc/supplementary_files/fsaMyaPrice.R`
#'
#'
#' @format  A data frame
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
