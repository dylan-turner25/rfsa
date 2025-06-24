# list all files in the PLC yields input data folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaPlcYields",
                    full.names = TRUE, pattern = "\\.(xlsx|xls)$")

# initialize a data frame to store PLC yields data
plc_yields <- NULL

# load and process each Excel file
for(file in files){
  # extract year from filename
  if(grepl("py([0-9]{4})", file)) {
    # For files like "plc_county_yields_py2023.xlsx"
    year <- as.numeric(gsub(".*py([0-9]{4}).*", "\\1", basename(file)))
  } else if(grepl("([0-9]{4})", file)) {
    # For files like "ccp_county_yields_2018.xls"
    year <- as.numeric(gsub(".*([0-9]{4}).*", "\\1", basename(file)))
  } else {
    warning(paste("Could not extract year from filename:", basename(file)))
    next
  }
  
  # read the first sheet of each file
  temp <- readxl::read_excel(file, sheet = 1)
  
  # handle different file formats based on year
  if(year <= 2018) {
    # 2018 and earlier format: CCP yields in wide format
    # Header is in row 1, units in row 2, data starts from row 3
    header_row <- temp[1, ]
    temp <- temp[-c(1:2), ]  # remove header and units rows
    
    # Set column names from header row
    colnames(temp) <- as.character(header_row)
    
    # Convert from wide to long format
    temp <- temp %>%
      tidyr::pivot_longer(
        cols = -c(1:3), # exclude ST_CTY, State Name, County Name
        names_to = "crop_name",
        values_to = "plc_yield"
      ) %>%
      dplyr::filter(!is.na(plc_yield) & plc_yield != "")
    
    # Rename columns to match newer format
    names(temp)[1:3] <- c("st_cty", "state_name", "county_name")
    
    # Add missing columns
    temp$crop_year <- year
    temp$enrolled_base <- NA
    temp$plc_yield_units <- "Bushel"  # Default unit for older files
    
  } else {
    # 2019+ format: long format with header in row 2
    # Remove first empty row
    temp <- temp[-1, ]
    
    # Set column names from row 1 (which is now the header)
    names(temp) <- c("state_code", "county_code", "state_name", "county_name", 
                     "crop_name", "crop_year", "enrolled_base", "plc_yield", "plc_yield_units")
    
    # Remove the header row
    temp <- temp[-1, ]
    
    # Create st_cty column (FIPS code)
    temp$st_cty <- paste0(temp$state_code, temp$county_code)
  }
  
  # remove any rows with all NA values
  temp <- temp[rowSums(is.na(temp)) != ncol(temp), ]
  
  # add program year
  temp$program_year <- year
  
  # ensure numeric columns are properly typed
  temp$plc_yield <- as.numeric(temp$plc_yield)
  temp$enrolled_base <- as.numeric(temp$enrolled_base)
  temp$program_year <- as.integer(temp$program_year)
  temp$crop_year <- as.integer(temp$crop_year)
  
  # filter out rows with missing yields
  temp <- temp %>%
    dplyr::filter(!is.na(plc_yield) & plc_yield > 0)
  
  # bind to master data frame
  plc_yields <- dplyr::bind_rows(plc_yields, temp)
}

# clean crop names (extract crop type first, then clean)
plc_yields$crop_type <- unlist(lapply(plc_yields$crop_name, extract_crop_type))

# add rma crop codes where applicable (before cleaning crop names)
plc_yields$rma_type_code <- unlist(lapply(plc_yields$crop_name, extract_crop_type, rma_code = TRUE))

# clean crop names (this removes crop type suffixes after extraction)
plc_yields$crop_name <- unlist(lapply(plc_yields$crop_name, clean_crop_names2))

# add rma crop codes (after cleaning)
plc_yields$rma_crop_code <- unlist(lapply(plc_yields$crop_name, assign_rma_cc))

# rename columns for consistency
plc_yields <- plc_yields %>%
  dplyr::rename(crop = crop_name,
                state = state_name,
                county = county_name,
                fips = st_cty) %>%
  dplyr::select(fips, state, county, crop, crop_type, plc_yield, plc_yield_units, 
                enrolled_base, program_year, crop_year, rma_crop_code, rma_type_code)

# type convert final columns
plc_yields <- readr::type_convert(plc_yields)

# convert to a tibble before exporting
fsaPlcYields <- dplyr::as_tibble(plc_yields)

# use the county level file in the package data folder
usethis::use_data(fsaPlcYields, overwrite = TRUE)