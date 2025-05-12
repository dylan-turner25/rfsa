

#' Loads data from FSA individual payment files for specified years and programs
#'
#' @param year numeric or character values specifying the years of interest (ex: `c(2020:2024)`)
#' @param program a character vector indicating the programs to load. Defaults to all programs.
#' @param year_type  one of "program", "payment", or "fiscal" which determines the year type to load. Defaults to "program".
#' @param aggregation One of "individual", "county", "state", or "national" which determines the level of aggregation.
#' The level of aggregation also determines the columns returned. For example, the state name would not be returned
#' if national level data was indicated.
#'
#' @returns a data frame
#' @importFrom dplyr summarize group_by
#' @importFrom purrr map_dfr
#' @import rlang
#' @export
#'
#' @examples \dontrun{get_fsa_payments(year = 2024,
#'                                     program = c("ARC-CO","PLC"),
#'                                     year_type = "program",
#'                                     aggregation = "county")}
get_fsa_payments <- function(year,
                             program = NULL,
                             year_type = "program",
                             aggregation = "national"){

  # path to directory containing payment files
  dir = paste0("./inst/extdata/fsaFarmPayments/",year_type,"_year_files")

  if(!is.null(program)){
  # check if program is valid
  all_programs <- gsub("\\.rds","",list.files(path = paste0(dir,"/2024")))


  for(p in program){
    if(!p %in% all_programs){
      stop(paste0("Program ", p, " is not a valid program. Valid programs are: ", paste(all_programs, collapse = ", ")))
    }
  }

  # get a vector of all the files to load
  files = c()
  for( y in year){
    files = c(files, paste0(y,"/",program,".rds"))
  }

  # add the directory
  files = paste0(dir,"/",files)
  } else {
    files = list.files(path = dir, pattern = "*.rds", full.names = TRUE, recursive = TRUE)
  }


  # load the files
  data <- files |>
    purrr::map_dfr(readRDS)


  # set column names
  colnames(data) <- c("state_cd_fsa","state_name_fsa","county_cd_fsa","county_name_fsa",
                      "name_payee","address_info","address_payee","city_payee","state_ab_payee","zip_payee",
                      "delivery_point_bar_code","payment_amount","payment_date","accounting_program_code",
                      "accounting_program_description","program_year","payment_year","fiscal_year",
                      "program_abb","program_name","fips_fsa")


  # determine aggregation level and return the appropriate data
  if(aggregation == "individual"){
    return(data)
  } else if (aggregation == "national"){
    data <- data |>
      group_by(.data[[paste0(year_type, "_year")]], .data$program_abb, .data$program_name) |>
      summarize(payment_amount = sum(.data$payment_amount, na.rm = T))
  } else if (aggregation == "state"){
    data <- data |>
      group_by(.data$state_cd_fsa, .data$state_name_fsa, .data[[paste0(year_type, "_year")]], .data$program_abb, .data$program_name) |>
      summarize(payment_amount = sum(.data$payment_amount, na.rm = T))
  } else if (aggregation == "county"){
    data <- data |>
      group_by(.data$fips_fsa, .data$state_cd_fsa, .data$county_cd_fsa, .data$county_name_fsa,
               .data[[paste0(year_type, "_year")]], .data$program_abb, .data$program_name) |>
      summarize(payment_amount = sum(.data$payment_amount, na.rm = T))
  } else {
    stop("Invalid aggregation type. Valid types are: individual, county, state, national")
  }
  return(data)
}

