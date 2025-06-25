
#' Get PLC Yield Data
#'
#' This internal helper function retrieves and filters PLC yield data from the
#' fsaPlcYields dataset based on crop, program year, and optional location parameters.
#' It handles flexible location input including state names, abbreviations, FIPS codes,
#' and county specifications.
#'
#' @param crop Character. The crop name to filter for (e.g., "corn", "soybeans").
#' @param program_year Numeric. The program year to filter for (e.g., 2024).
#' @param state Character, optional. State identifier that can be:
#'   \itemize{
#'     \item State abbreviation (e.g., "IA", "IL")
#'     \item Full state name (e.g., "Iowa", "Illinois")
#'     \item State FIPS code (e.g., "19", "17")
#'   }
#' @param county Character, optional. County name (requires state to also be specified).
#' @param fips Character, optional. 5-digit FIPS code for specific county-level data.
#'
#' @return Numeric. The PLC yield value. Returns:
#'   \itemize{
#'     \item County-specific yield if fips or state+county provided
#'     \item State average yield if only state provided (with message)
#'     \item National average yield if no location specified (with message)
#'   }
#'
#' @details
#' The function follows this priority order for location filtering:
#' \enumerate{
#'   \item If \code{fips} is provided, uses it directly after validation
#'   \item If \code{state} and \code{county} are provided, filters by both
#'   \item If only \code{state} is provided, calculates state average
#'   \item If no location parameters, calculates national average
#' }
#'
#' State abbreviations are automatically converted to full state names since the
#' underlying data uses full state names for filtering.
#'
#' @examples
#' \dontrun{
#' # County-specific yield using FIPS
#' get_plc_yield("corn", 2024, fips = "19001")
#'
#' # County-specific yield using state + county
#' get_plc_yield("corn", 2024, state = "IA", county = "Adair")
#'
#' # State average (abbreviation converted to full name)
#' get_plc_yield("corn", 2024, state = "IA")
#'
#' # National average
#' get_plc_yield("corn", 2024)
#' }
#'
#' @keywords internal
get_plc_yield <- function(crop, program_year, state = NULL, county = NULL, fips = NULL) {
  data("fsaPlcYields")

  # filter on crop and marketing year
  # note: plc average county yields aren't avaliable prior to 2018, so function defaults to 2018 for earlier years
  plc_yield = fsaPlcYields %>%
    dplyr::filter(.data$crop == .env$crop,
                  .data$program_year == ifelse(.env$program_year <= 2018, 2018, .env$program_year) )

  # filter the yield data based on location input
  if (!is.null(fips)) {
    # Use fips if provided - clean and validate using clean_fips approach
    fips_clean <- clean_fips(fips = fips)
    plc_yield <- plc_yield %>% dplyr::filter(.data$fips == fips_clean)
    warning("No PLC yield supplied. Using counuty average PLC yield for calculations.")
  } else if (!is.null(state)) {
    # Validate state input (accepts names, abbreviations, or fips codes)
    state <- valid_state(state)

    # Convert state abbreviation to full name if needed (since data has full names)
    if (tolower(state) %in% tolower(datasets::state.abb)) {
      # Convert abbreviation to full name
      state_index <- which(tolower(datasets::state.abb) == tolower(state))
      state_name <- datasets::state.name[state_index]
    } else if (tolower(state) %in% tolower(datasets::state.name)) {
      # State is already a full name
      state_name <- state
    } else if (nchar(state) <= 2 && suppressWarnings(!is.na(as.numeric(state)))) {
      # State is a fips code - need to convert to state name
      state_fips <- sprintf("%02d", as.numeric(state))
      # Get state name from fips code
      fips_to_state <- data.frame(
        fips = usmap::fips(state = datasets::state.name),
        name = datasets::state.name
      )
      state_match <- fips_to_state[fips_to_state$fips == state_fips, "name"]
      if (length(state_match) == 0) {
        stop("Invalid state fips code: ", state)
      }
      state_name <- state_match[1]
    } else {
      stop("Invalid state input: ", state)
    }

    if (!is.null(county)) {
      # Both state and county provided - filter by state name and county
      plc_yield <- plc_yield %>% dplyr::filter(.data$state == state_name, .data$county == county)
    } else {
      # Only state provided - filter by state name and average
      plc_yield <- plc_yield %>% dplyr::filter(.data$state == state_name) %>%
        summarize(plc_yield = mean(.data$plc_yield, na.rm = TRUE))
      warning("No PLC yield or county supplied. Using state average PLC yield for calculations.")
    }
  } else {
    # No location filtering - use national average
    warning("No PLC yield or location parameters supplied. Using national average PLC yield for calculations.")
    plc_yield <- plc_yield %>%
      summarize(plc_yield = mean(.data$plc_yield, na.rm = TRUE))  # Average yield for national
  }

  return(plc_yield$plc_yield)
}

#' Calculate Effective Reference Price (Internal Function)
#'
#' This internal helper function calculates the effective reference price (ERP) using
#' the Olympic average method based on 5 years of Marketing Year Average (MYA) prices
#' and the statutory reference price.
#'
#' @param mya_prices Numeric vector. Five years of MYA prices for Olympic average calculation.
#' @param srp Numeric. The statutory reference price for the crop.
#'
#' @return Numeric. The calculated effective reference price based on the formula:
#'   \itemize{
#'     \item If 85% of Olympic average < SRP, then ERP = SRP
#'     \item If 85% of Olympic average >= 115% of SRP, then ERP = 115% of SRP  
#'     \item Otherwise, ERP = 85% of Olympic average
#'   }
#'
#' @details
#' The Olympic average removes the highest and lowest values from the 5 MYA prices
#' and averages the remaining 3 middle values. The ERP is then calculated as 85%
#' of this Olympic average, subject to minimum and maximum bounds relative to the SRP.
#'
#' @examples
#' \dontrun{
#' # Calculate ERP with 5 years of MYA prices
#' mya_prices <- c(3.70, 3.60, 3.50, 3.40, 3.30)
#' srp <- 3.90
#' calc_effective_reference_price(mya_prices, srp)
#' }
#'
#' @keywords internal
calc_effective_reference_price <- function(mya_prices, srp) {
  # Validate inputs
  if(length(mya_prices) != 5) {
    stop("Exactly 5 MYA prices are required for ERP calculation")
  }
  if(is.null(srp) || !is.numeric(srp)) {
    stop("Statutory reference price (srp) must be a numeric value")
  }
  
  # Calculate Olympic average (remove highest and lowest, average the rest)
  sorted_prices <- sort(mya_prices)
  olympic_avg <- mean(sorted_prices[2:(length(sorted_prices)-1)])
  
  # Apply ERP formula
  if(0.85 * olympic_avg < srp) {
    erp <- srp
  } else if(0.85 * olympic_avg >= 1.15 * srp) {
    erp <- srp * 1.15
  } else {
    erp <- 0.85 * olympic_avg
  }
  
  return(erp)
}

#' List asset names from the latest GitHub release
#'
#' Retrieves the metadata for the most recent release of the **rfsa** repository
#' on GitHub and extracts the names of all attached release assets.
#'
#' @return A character vector of file names (assets) in the latest release.
#' @keywords internal
#' @examples
#' \dontrun{
#' files = list_data_assets()
#' }
#' @importFrom gh gh
list_data_assets <- function(){
  # 1. Fetch the release metadata (by tag, or "latest")
  release <- gh::gh(
    "/repos/{owner}/{repo}/releases/latest",
    owner = "dylan-turner25",
    repo  = "rfsa"
  )

  # 2. Extract the assets list
  assets <- release$assets

  # 3. Pull out the bits you care about
  df <- data.frame(
    name = vapply(assets, `[[`, "", "name"),
    url  = vapply(assets, `[[`, "", "browser_download_url"),
    size = vapply(assets, `[[`, 0,  "size"),
    stringsAsFactors = FALSE
  )

  return(df$name)
}

#' A helper function that checks to make sure the specified state is valid
#' @noRd
#' @keywords internal
#'
valid_state <- function(state) {
  if (!(F %in% (tolower(state) %in% tolower(datasets::state.name)))) {
    return(state)
  } else if (!(F %in% (tolower(state) %in% tolower(datasets::state.abb)))) {
    return(state)
  } else if (!(F %in% (suppressWarnings(as.numeric(state) %in% as.numeric(usmap::fips(state = datasets::state.name)))))) {
    return(state)
  } else {
    stop("Parameter value for state not valid.")
  }
}

#' @title Download a data file from GitHub Releases via piggyback
#' @param name   The basename of the .rds file, e.g. "foo.rds"
#' @param tag    Which release tag to download from (default: latest)
#' @return       The local path to the downloaded file
#' @keywords internal
#' @noRd
#' @import piggyback
get_cached_rds <- function(name,
                           repo = "dylan-turner25/rfsa",
                           tag  = NULL) {
  dest_dir <- tools::R_user_dir("rfsa", which = "cache")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  dest_file <- file.path(dest_dir, name)
  if (!file.exists(dest_file)) {
    # download from the Release
   piggyback::pb_download(
      file     = name,
      repo     = repo,
      tag      = tag,
      dest = dest_dir
    )
  }
  readRDS(dest_file)
}

#' Clear the package cache of downloaded RDS files
#'
#' Deletes the entire cache directory used by the **rfsa** package to store
#' downloaded \*.rds files. Useful if you need to force re-download of data,
#' or free up disk space.
#'
#' @return Invisibly returns `NULL`. A message is printed indicating which
#'   directory was cleared.
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove all cached RDS files so they will be re-downloaded on next use
#' clear_rfsa_cache()
#' }
clear_rfsa_cache <- function(){
  dest_dir <- tools::R_user_dir("rfsa", which = "cache")
  if (dir.exists(dest_dir)) {
    unlink(dest_dir, recursive = TRUE, force = TRUE)
  }
  message("Cleared cached files in ", dest_dir)
  invisible(NULL)
}
#' Splits FSA individual payment files into program/state files
#'
#' @param data a data frame containing FSA payment data
#' @param year_type a character string indicating the type of year to split by.
#'
#' @return splits data into subsets by payment year and FSA program then saves them to the
#' relevant rds file
#' @noRd
#' @keywords internal
#' @import dplyr rlang
#'
split_file <- function(data, year_type){

  # merge duplicate entries into a single transaction
  data <- data.frame(data %>%
    group_by(across(-.data$disbursement_amount)) %>%
    summarise(disbursement_amount = sum(.data$disbursement_amount, na.rm = TRUE),
              .groups = "drop"))

  # convert entire data frame to character
  data <- data %>% mutate(across(everything(), as.character))


  if(year_type == "program"){
    year_var <- "accounting_program_year"
  }
  if(year_type == "payment"){
    year_var <- "payment_year"
  }
  if(year_type == "fiscal"){
    year_var <- "fiscal_year"
  }

  years    <- unique(data[,year_var])
  programs <- unique(data$prog_abb)

  for(y in years){
    for(p in programs){

      # ensure names are portables
      p <- gsub(" ","-",p)

      #print( paste0(year_type," year ",y,"-",p) )
      # make sure folder exists
      dir <- file.path("data-raw","fsaFarmPayments","output_data",
                       paste0(year_type, "_year_files"), y)

      #print(dir)
      if(!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
      }

      file_name <- file.path(dir, paste0(year_type,"_",y,"_",p, ".rds"))
      #print(file_name)

      new_observations <- switch(
        year_type,
        payment = filter(data, .data$prog_abb     == p, .data$payment_year == y),
        fiscal  = filter(data, .data$prog_abb     == p, .data$fiscal_year  == y),
        program = filter(data, .data$prog_abb     == p, .data$accounting_program_year == y),
        stop("Unknown year_type")
      )

      # if file_name does not exist, save new observations as file name
      # otherwise read in existing file and merge with new observations before saving
      if(!file.exists(file_name)){
        saveRDS(new_observations, file_name)
      } else {
        existing_observations <- readRDS(file_name)
        merged_observations   <- dplyr::distinct(bind_rows(existing_observations, new_observations))
        saveRDS(merged_observations, file_name)
      }
    }
  }

}


#' Clean fips codes or construct them from state and county codes
#'
#' If a string is passed to fips, the function will convert the fips codes
#' to 5 digit codes (in the case where 3 or 4 digit codes are present)
#'
#' If state and county fips codes are passed to the county and state arguemnts,
#' a 5 digit fips code will be constructed
#'
#' @param fips a character vector representing a fips codes
#' @param county a character vector representing a county fips code
#' @param state a character vector representing a state fips code
#'
#' @return returns a character vector containing 5 digit fips codes
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' clean_fips(fips = "1001")
#' clean_fips(county = "1", state = "1")
#'
clean_fips <- Vectorize(function(fips = NULL,county = NULL,state = NULL){


  if(is.null(fips)){
    if(nchar(state) == 1){
      state_clean <- paste0("0",state)
    }
    if(nchar(state) == 2){
      state_clean <- as.character(state)
    }
    if(nchar(county) == 1){
      county_clean <- paste0("00",county)
    }
    if(nchar(county) == 2){
      county_clean <- paste0("0",county)
    }
    if(nchar(county) == 3){
      county_clean <- as.character(county)
    }
    fips <- paste0(state_clean,county_clean)
    return(fips)
  }
  if(is.null(fips) == F){
    if(nchar(fips) == 4){
      fips_clean <- paste0("0",fips)
    }
    if(nchar(fips) < 4){
      print("Warning: fips codes should be either 4 or 5 character strings. Returning NA")
      return(NA)
    }
    if(nchar(fips) == 5){
      fips_clean <- as.character(fips)
    }
    return(fips_clean)
  }
})

#' Clean and standardize column names for FSA MYA Price files
#'
#' Takes a data frame of FSA "MYA Price" data and replaces fiscal-year patterns
#' and other inconsistent suffixes with a uniform T-minus scheme, plus standardizes
#' "Prices" to singular "Price" and final/projection markers.
#'
#' @param df A data.frame containing FSA MYA Price columns named with patterns like "2014/15 Prices".
#' @param year Integer. The calendar year corresponding to the most recent data in `df`;
#'   used to generate T-minus replacements back to `year - 10`.
#'
#' @return A character vector of cleaned column names.
#' @noRd
#' @keywords internal
rename_mya_cols <- function(df, year) {
  replacement <- 0
  for (y in seq(year, year - 10)) {
    pattern <- paste0(y, "/", substr(y + 1, 3, 4))
    colnames(df) <- gsub(pattern, paste0("T - ", replacement), colnames(df))
    replacement <- replacement + 1
  }
  colnames(df) <- gsub("Prices", "Price", colnames(df))
  colnames(df) <- gsub("Projected \\(P\\) or Final \\(F\\)", "Final", colnames(df))
  colnames(df) <- gsub("Projected \\(P\\) Final \\(F\\)", "Final", colnames(df))

  return(colnames(df))
}

#' Extract type of a crop from its name
#'
#' Given a human-readable crop name string, returns the grain type
#' (e.g. "long grain", "short/medium grain") or cotton seed indicator.
#'
#' @param crop_name Character. The commodity name as found in FSA or RMA outputs.
#' @param rma_code Logical. If `TRUE`, returns the RMA internal code string;
#'   otherwise returns a descriptive label.
#'
#' @return Character or `NA`. If `rma_code = FALSE`, one of "long grain",
#'   "short/medium grain", "temperate japonica", "seed", "large", "small";
#'   if `rma_code = TRUE`, the corresponding RMA code string (e.g. "453", "451/452");
#'   returns `NA` if no match.
#' @noRd
#' @keywords internal
extract_crop_type <- function(crop_name, rma_code = FALSE) {

  # cotton seed vs lint not coded
  if (grepl("cotton", tolower(crop_name))) {
    if (grepl("seed", tolower(crop_name))) {
      return(if (rma_code) NA_character_ else "seed")
    }
    if (grepl("upland", tolower(crop_name))) {
      return(if (rma_code) NA_character_ else "upland")
    }
  }

  # rice types
  if (grepl("rice", tolower(crop_name))) {
    if (grepl("long", tolower(crop_name))) {
      return(if (rma_code) "453" else "long grain")
    } else if (grepl("medium|short", tolower(crop_name))) {
      return(if (rma_code) "451/452" else "short/medium grain")
    } else if (grepl("japonica", tolower(crop_name))) {
      return(if (rma_code) "451/452" else "temperate japonica")
    }
  }

  # chickpeas
  if (grepl("chickpea", tolower(crop_name))) {
    if (rma_code) return(NA_character_)
    if (grepl("large", tolower(crop_name))) {
      return("large")
    } else if (grepl("small", tolower(crop_name))) {
      return("small")
    }
  }

  # no match
  return(NA_character_)
}

#' Standardize free-text crop names
#'
#' Remove footnote indicators, parentheses, size qualifiers, and trim whitespace.
#'
#' @param crop_name Character. The raw crop name possibly containing notes
#'   or parentheses.
#'
#' @return Character. The cleaned crop name in lowercase without annotations.
#' @noRd
#' @keywords internal
clean_crop_names <- function(crop_name) {
  cleaned_name <- gsub("[0-9]/", "", crop_name)
  cleaned_name <- tolower(cleaned_name)
  cleaned_name <- gsub("\\s*\\([^)]*\\)", "", cleaned_name)
  cleaned_name <- gsub("\\blarge\\b|\\bsmall\\b|\\bseed\\b|\\bupland\\b", "", cleaned_name)
  cleaned_name <- trimws(cleaned_name)
  return(cleaned_name)
}


#' An upgraded version of `clean_crop_names` that can handle weird abbreviations
#'
#' Remove footnote indicators, parentheses, size qualifiers, and trim whitespace.
#' Also ccorrects abbreviated names to their full form.
#'
#' @param crop_name Character. The raw crop name possibly containing notes
#'   or parentheses.
#'
#' @return Character. The cleaned crop name in lowercase without annotations.
#' @noRd
#' @keywords internal
clean_crop_names2 <- function(crop_name) {
  if (is.na(crop_name)) return(NA_character_)

  # Remove numeric patterns and slash
  cleaned_name <- gsub("[0-9]/", "", crop_name)

  # Convert to lowercase
  cleaned_name <- tolower(cleaned_name)

  # Remove text in parentheses
  cleaned_name <- gsub("\\s*\\([^)]*\\)", "", cleaned_name)

  # Remove specific words
  cleaned_name <- gsub("\\blarge\\b|\\bsmall\\b|\\bseed\\b|\\bupland\\b", "", cleaned_name)

  # Additional cleaning
  cleaned_name <- gsub(" - blank$", "", cleaned_name)
  cleaned_name <- gsub(" - .*grain$", "", cleaned_name)
  cleaned_name <- gsub(" - temporate japonica$", "", cleaned_name)
  cleaned_name <- gsub(" - garbanzo-lg kabuli$| - garbanzo-sm desi$", "", cleaned_name)

  # Remove common crop type suffixes
  cleaned_name <- gsub("\\s+lint$", "", cleaned_name)  # "cotton lint" -> "cotton"
  cleaned_name <- gsub("\\bmed/short grain$|\\blong grain$|\\bmedium grain$|\\bshort grain$", "", cleaned_name)
  cleaned_name <- gsub("\\btemperate japonica$|\\btropical japonica$", "", cleaned_name)
  cleaned_name <- gsub("\\blarge kabuli$|\\bsmall desi$", "", cleaned_name)
  cleaned_name <- gsub("\\boil type$|\\bconfection type$", "", cleaned_name)
  cleaned_name <- gsub("\\bdark northern spring$|\\bhard red winter$|\\bsoft red winter$|\\bwhite$|\\bdurum$", "", cleaned_name)

  # Standardize abbreviated names
  lookup <- c(
    "barly" = "barley",
    "pnuts" = "peanuts",
    "snflr" = "sunflowers",
    "sflwr" = "safflower",
    "sorgh" = "sorghum",
    "soybn" = "soybeans",
    "canol" = "canola",
    "sesme" = "sesame",
    "lenti" = "lentils",
    "mustd" = "mustard",
    "rape" = "rapeseed",
    "cramb" = "crambe"
  )

  for (abbr in names(lookup)) {
    if (cleaned_name == abbr) {
      cleaned_name <- lookup[abbr]
      break
    }
  }

  # Final trim of whitespace
  cleaned_name <- trimws(cleaned_name)

  # Remove trailing hyphens (fixes cases like "cotton-" from "seed cotton-upland")
  cleaned_name <- gsub("-+$", "", cleaned_name)

  return(cleaned_name)
}

#' Assign RMA commodity codes to crop names
#'
#' Maps a crop name string to its RMA numeric commodity code.
#'
#' @param crop_name Character. The commodity name to match.
#'
#' @return Integer or `NA`. The RMA commodity code (e.g. 41, 81, 11, etc.),
#'   or `NA` if the crop is not in the predefined mapping.
#' @noRd
#' @keywords internal
assign_rma_cc <- function(crop_name) {
  name_lc <- tolower(crop_name)
  if (grepl("corn", name_lc)) {
    return(41)
  } else if (grepl("soybean", name_lc)) {
    return(81)
  } else if (grepl("wheat", name_lc)) {
    return(11)
  } else if (grepl("rice", name_lc)) {
    return(18)
  } else if (grepl("cotton", name_lc)) {
    return(21)
  } else if (grepl("sorghum", name_lc)) {
    return(51)
  } else if (grepl("barley", name_lc)) {
    return(91)
  } else if (grepl("oat", name_lc)) {
    return(16)
  } else if (grepl("peanut", name_lc)) {
    return(75)
  } else if (grepl("canola|rape", name_lc)) {
    return(15)
  } else if (grepl("sunflower", name_lc)) {
    return(78)
  } else if (grepl("dry pea|chickpea|lentil", name_lc)) {
    return(67)
  } else if (grepl("flaxseed|linseed", name_lc)) {
    return(31)
  } else if (grepl("mustard seed|mustard", name_lc)) {
    return(69)
  } else if (grepl("safflower", name_lc)) {
    return(49)
  } else if (grepl("potato|sweet potato", name_lc)) {
    return(156)
  } else if (grepl("sesame", name_lc)) {
    return(396)
  } else {
    return(NA_integer_)
  }
}




#' Convert POSIX date object to U.S. Government fiscal year
#'
#' @param date A calendar date formatted as a posix object
#'
#' @return returns a numeric value for the Government fiscal year corresponding
#' to the POSIX date
#' @noRd
#' @keywords internal
#'
get_fiscal_year <- function(date){

  # extract the calendar year from the date input
  year <- as.numeric(format(date, format="%Y"))

  # if date is between october 1st of last year and september 30th of this year
  # return the current calendar year
  if(date >=  as.Date(paste0((year - 1),"-10-01"),"%Y-%m-%d") &
     date <=   as.Date(paste0((year),"-09-30"),"%Y-%m-%d")) {
    return(year)
  }

  # if date is between october 1st of this calendar year and september 30th of
  # next calender year, return the current calendar year + 1
  if(date >=  as.Date(paste0((year),"-10-01"),"%Y-%m-%d") &
     date <=   as.Date(paste0((year + 1),"-09-30"),"%Y-%m-%d")) {
    return(year + 1)
  }

}




