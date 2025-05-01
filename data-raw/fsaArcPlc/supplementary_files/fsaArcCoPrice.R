files <- list.files("./data-raw/fsaArcPlc/input_data/fsaArcCoPrice",
                    full.names = T)

# initialize a data frame to store arc-ic prices
arc_co_prices <- NULL

# load the files and bind them into one data frame
for(year in 2014:(current_year - 1)){
  # get file path corresponding to year
  file <- files[grepl(year,files)]

  # load the file to a temporary object
  temp <- readxl::read_excel(file)

  # locate row containing "Commodity" which contains column names
  commodity_row <- which(temp[,1] == "Commodity")
  for( k in 1:ncol(temp)){
    if(is.na(temp[commodity_row + 1,k])){
      temp[commodity_row + 1,k] <- temp[commodity_row ,k]
    }
  }

  # define column names
  colnames(temp) <- temp[commodity_row + 1,]

  # remove everything above the commodity_row
  temp <- temp[-c(1:(commodity_row + 1)),]

  # clean the raw data
  temp$year <- paste0(year,"-",year+1)
  temp <- temp[,which(colnames(temp) != "NA")]

  if("MAX" %in% colnames(temp)){
    temp <- temp %>% select(-MAX,-MIN)
  }

  temp <- temp %>% na.omit() # omit NA and remove max and min columns


  if(year == 2014){
    for(year_temp in 2000:2030){
      colnames(temp) <- gsub(paste0(year_temp,"/",substr(year_temp + 1, 3,4),"|",year_temp),paste0("T-",year - year_temp),colnames(temp))
    }
    colnames(temp) <- trimws(gsub("4/|3/|/1","",colnames(temp)))
    #colnames(temp) <- gsub(paste0(year,"/",substr(year + 1, 3,4),"|",year),"T-0",colnames(temp))
    colnames(temp) <- trimws(gsub("Projected|\\(P\\)|\\(F\\)| or ","",colnames(temp)))
    colnames(temp) <- trimws(gsub("  "," ",colnames(temp)))
  } else {
    colnames(temp) <- colnames(arc_co_prices)
  }

  # if plc_payment_rates is null, define it
  if(is.null(arc_co_prices)){
    arc_co_prices <- temp
  } else {
    arc_co_prices <- dplyr::bind_rows(arc_co_prices, temp)
  }

}

# rename columns
names(arc_co_prices) <- c("crop","marketing_year_dates",
                           "publishing_dates_for_final_mya_price",
                           "unit","reference_price_combined",
                           "annual_benchmark_price_lag5",
                           "annual_benchmark_price_lag4",
                           "annual_benchmark_price_lag3",
                           "annual_benchmark_price_lag2",
                           "annual_benchmark_price_lag1",
                           "current_arcco_benchmark_price",
                           "current_mya_price",
                           "current_national_loan_rate",
                           "current_arcco_actual_price",
                           "marketing_year")

# add program_year
arc_co_prices$program_year <- substr(arc_co_prices$marketing_year,1,4)

# extract crop type
arc_co_prices$crop_type <- unlist(lapply(arc_co_prices$crop, extract_crop_type))

# add rma crop codes where applicable
arc_co_prices$rma_type_code <- unlist(lapply(arc_co_prices$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
arc_co_prices$crop <- unlist(lapply(arc_co_prices$crop, clean_crop_names))

# add rma crop codes
arc_co_prices$rma_crop_code <- unlist(lapply(arc_co_prices$crop, assign_rma_cc))

# type convert the columns
arc_co_prices <- readr::type_convert(arc_co_prices)

# convert the fsaFarmPayments data to a tibble before exporting
fsaArcCoPrice <- dplyr::as_tibble(arc_co_prices)

# use export the data to the the package data folder
usethis::use_data(fsaArcCoPrice, overwrite = TRUE)


