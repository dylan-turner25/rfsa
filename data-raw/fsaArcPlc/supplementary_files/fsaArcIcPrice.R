# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaArcIcPrice",
                    full.names = T)

# initialize a data frame to store arc-ic prices
arc_ic_prices <- NULL


# load the files and bind them into one data frame
for(year in 2015:(current_year - 1)){
  # get file path corresponding to year
  file <- files[grepl(year,files)]

  # load the file to a temporary object
  #try(temp <- readxl::read_xls(file))
  #try(temp <- readxl::read_xlsx(file))
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
  #colnames(temp) <- temp[3,]
  #temp <- temp[-c(1:4),]
  #temp <- temp[-c(which(is.na(temp$Commodity)):nrow(temp)),]
  temp$year <- paste0(year,"-",year+1)
  #colnames(temp)[3] <- "Publishing Dates for the  Final T-0 PLC Effective Price"
  temp <- temp[,which(colnames(temp) != "NA")] %>% na.omit() # omit NA

  if(year == 2015){
    for(year_temp in 2010:2030){
      colnames(temp) <- gsub(paste0(year_temp,"/",substr(year_temp + 1, 3,4),
                                    "|",year_temp),paste0("T-",year - year_temp)
                             ,colnames(temp))
    }
    colnames(temp) <- trimws(gsub("3/|/1","",colnames(temp)))
    colnames(temp) <- trimws(gsub("Projected|\\(P\\)|\\(F\\)| or ",
                                  "",colnames(temp)))
    colnames(temp) <- trimws(gsub("  "," ",colnames(temp)))
  } else {
    colnames(temp) <- colnames(arc_ic_prices)
  }

  # if plc_payment_rates is null, define it
  if(is.null(arc_ic_prices)){
    arc_ic_prices <- temp
  } else {
    arc_ic_prices <- dplyr::bind_rows(arc_ic_prices, temp)
  }

}

# rename columns
names(arc_ic_prices) <-  c("crop","marketing_year_dates",
                           "publishing_dates_for_final_mya_price",
                           "unit","reference_price_combined",
                           "annual_benchmark_price_lag5",
                           "annual_benchmark_price_lag4",
                           "annual_benchmark_price_lag3",
                           "annual_benchmark_price_lag2",
                           "annual_benchmark_price_lag1",
                           "current_mya_price",
                           "current_national_loan_rate",
                           "current_arcic_actual_price",
                           "marketing_year")



# add program_year
arc_ic_prices$program_year <- substr(arc_ic_prices$marketing_year,1,4)

# extract crop type
arc_ic_prices$crop_type <- unlist(lapply(arc_ic_prices$crop, extract_crop_type))

# add rma crop codes where applicable
arc_ic_prices$rma_type_code <- unlist(lapply(arc_ic_prices$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
arc_ic_prices$crop <- unlist(lapply(arc_ic_prices$crop, clean_crop_names))

# add rma crop codes
arc_ic_prices$rma_crop_code <- unlist(lapply(arc_ic_prices$crop, assign_rma_cc))

# type convert the columns
arc_ic_prices <- readr::type_convert(arc_ic_prices)

# convert the data to a tibble before exporting
fsaArcIcPrice <- dplyr::as_tibble(arc_ic_prices)

# use the county level file in the package data folder
usethis::use_data(fsaArcIcPrice, overwrite = TRUE)

