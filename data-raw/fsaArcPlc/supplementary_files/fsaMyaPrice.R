# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaMyaPrice",
                    full.names = T)

# initialize a data frame to store mya prices
mya_prices <- NULL

# load the files and bind them into one data frame
for(year in 2014:(current_year - 1)){
  # get file path corresponding to year
  file <- files[grepl(year,files)]

  # load the file to a temporary object
  #try(temp <- readxl::read_xls(file))
  #try(temp <- readxl::read_xlsx(file))
  temp <- readxl::read_excel(file)


  # clean the raw data
  colnames(temp) <- temp[3,]
  temp <- temp[-c(1:4),]
  temp <- temp[-c(which(is.na(temp$Commodity)):nrow(temp)),]
  temp$year <- paste0(year,"-",year+1)
  colnames(temp)[3] <- "Publishing Dates for the  Final T - 0 MYA Price"
  colnames(temp) <- rename_mya_cols(temp,year = year)
  if("Final T - 6 MYA Price" %in% colnames(temp)){
    temp <- temp[,c("Commodity","Marketing Year", "year",
                    "Publishing Dates for the  Final T - 0 MYA Price","Unit",
                    "Final T - 6 MYA Price",
                    "Final T - 5 MYA Price",
                    "Final T - 4 MYA Price",
                    "Final T - 3 MYA Price",
                    "Final T - 2 MYA Price",
                    "Final T - 1 MYA Price",
                    "Final T - 0 MYA Price")]
  } else {
    temp <- temp[,c("Commodity","Marketing Year", "year",
                    "Publishing Dates for the  Final T - 0 MYA Price","Unit",
                    "Final T - 5 MYA Price",
                    "Final T - 4 MYA Price",
                    "Final T - 3 MYA Price",
                    "Final T - 2 MYA Price",
                    "Final T - 1 MYA Price",
                    "Final T - 0 MYA Price")]
  }

  # if mya_prices is null, define it
  if(is.null(mya_prices)){
    mya_prices <- temp
  } else {
    mya_prices <- dplyr::bind_rows(mya_prices, temp)
  }
}

# final cleaning operations

# strip any number followed by a "/" from the commodity name
mya_prices$Commodity <- gsub("[0-9]/","",mya_prices$Commodity)

# type convert the columns
mya_prices <- readr::type_convert(mya_prices)

# rename columns
names(mya_prices) <- c("crop","marketing_year_dates","marketing_year",
                       "publishing_dates_for_final_mya_price",
                       "unit",
                       "final_mya_price_lag5",
                       "final_mya_price_lag4",
                       "final_mya_price_lag3",
                       "final_mya_price_lag2",
                       "final_mya_price_lag1",
                       "current_mya_price",
                       "final_mya_price_lag6")

# extract crop type
mya_prices$crop_type <- unlist(lapply(mya_prices$crop, extract_crop_type))

# add rma crop codes where applicable
mya_prices$rma_type_code <- unlist(lapply(mya_prices$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
mya_prices$crop <- unlist(lapply(mya_prices$crop, clean_crop_names))

# add rma crop codes
mya_prices$rma_crop_code <- unlist(lapply(mya_prices$crop, assign_rma_cc))

# reorder columns so lags are in  order
mya_prices <- mya_prices[,c("crop",
                             "marketing_year",
                             "marketing_year_dates",
                             "publishing_dates_for_final_mya_price",
                             "unit",
                             "current_mya_price",
                             "final_mya_price_lag1",
                             "final_mya_price_lag2",
                             "final_mya_price_lag3",
                             "final_mya_price_lag4",
                             "final_mya_price_lag5",
                             "final_mya_price_lag6",
                             "rma_crop_code",
                             "crop_type",
                             "rma_type_code")]



# convert the data to a tibble before exporting
fsaMyaPrice <- dplyr::as_tibble(mya_prices)


# use the county level file in the package data folder
usethis::use_data(fsaMyaPrice, overwrite = TRUE)

