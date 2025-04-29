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
  cleaned_name <- gsub("\\blarge\\b|\\bsmall\\b|\\bseed\\b", "", cleaned_name)
  cleaned_name <- trimws(cleaned_name)
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
