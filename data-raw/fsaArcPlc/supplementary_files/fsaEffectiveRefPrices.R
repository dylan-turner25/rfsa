# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaEffectiveRefPrices",
                    pattern = "^ERP_[0-9]{4}\\.xlsx?$",
                    full.names = TRUE, recursive = FALSE)

# The ERP release can include a later year than the rest of the build.
erp_prices <- dplyr::bind_rows(lapply(files, function(file) {
  year <- as.integer(sub("^ERP_([0-9]{4})\\.xlsx?$", "\\1", basename(file)))
  temp <- readxl::read_excel(file)

  # locate row containing "Commodity" which contains column names
  commodity_row <- which(temp[,1] == "Commodity")

  # define column names
  colnames(temp) <- temp[commodity_row,]

  # remove everything above the commodity_row
  # Check if there's an empty row after commodity row, if so remove it too
  if (commodity_row + 1 <= nrow(temp) && is.na(temp[commodity_row + 1, 1])) {
    temp <- temp[-c(1:(commodity_row+1)),]
  } else {
    temp <- temp[-c(1:commodity_row),]
  }

  temp <- na.omit(temp[, which(names(temp) != "NA")])
  # Older workbooks show the high and low prices in separate columns.
  temp <- temp[, !names(temp) %in% c("MAX", "MIN")]
  multiplier_header <- grep("^[0-9]+% of 5-year avg", trimws(names(temp)), value = TRUE)
  if (ncol(temp) != 12L || length(multiplier_header) != 1L) {
    stop("Unexpected ERP columns in ", basename(file))
  }
  names(temp) <- c("crop", "marketing_year_dates", "unit",
                   "statutory_reference_price", "115_statutory_reference_price",
                   "mya_price_lag5", "mya_price_lag4", "mya_price_lag3",
                   "mya_price_lag2", "mya_price_lag1", "85_olympic_average_mya",
                   "effective_reference_price")
  temp$marketing_year <- paste0(year, "-", year + 1L)
  temp$program_year <- as.character(year)
  temp$olympic_average_multiplier <- readr::parse_number(multiplier_header) / 100
  temp
}))

# reorder columns
erp_prices <- erp_prices[,c("crop","marketing_year_dates",
                            "marketing_year","program_year",
                              "unit","statutory_reference_price",
                              "115_statutory_reference_price",
                              "mya_price_lag5", "mya_price_lag4",
                              "mya_price_lag3", "mya_price_lag2",
                              "mya_price_lag1","85_olympic_average_mya",
                              "effective_reference_price",
                              "olympic_average_multiplier")]

# extract crop type
erp_prices$crop_type <- unlist(lapply(erp_prices$crop, extract_crop_type))

# add rma crop codes where applicable
erp_prices$rma_type_code <- unlist(lapply(erp_prices$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
erp_prices$crop <- unlist(lapply(erp_prices$crop, clean_crop_names))

# add rma crop codes
erp_prices$rma_crop_code <- unlist(lapply(erp_prices$crop, assign_rma_cc))


# type convert columns
erp_prices <- readr::type_convert(erp_prices)

# convert the fsaEffectiveRefPrices data to a tibble before exporting
fsaEffectiveRefPrices <- dplyr::as_tibble(erp_prices)


# use the county level file in the package data folder
usethis::use_data(fsaEffectiveRefPrices, overwrite = TRUE)


