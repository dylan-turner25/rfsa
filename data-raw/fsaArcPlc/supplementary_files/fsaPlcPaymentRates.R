# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaPlcPaymentRate",
                    full.names = T)

# initialize a data frame to store mya prices
plc_payment_rates <- NULL

# load the files and bind them into one data frame
for(year in 2014:(current_year - 1)){
  # get file path corresponding to year
  file <- files[grepl(year,files)]

  # load the file to a temporary object
  #try(temp <- readxl::read_xls(file))
  #try(temp <- readxl::read_xlsx(file))
  temp <- readxl::read_excel(file)

  # locate row containing "Commodity" which contains column names
  commodity_row <- which(temp[,1] == "Commodity")

  # define column names
  colnames(temp) <- temp[commodity_row,]

  # remove everything above the commodity_row
  temp <- temp[-c(1:commodity_row),]

  # clean the raw data
  #colnames(temp) <- temp[3,]
  #temp <- temp[-c(1:4),]
  #temp <- temp[-c(which(is.na(temp$Commodity)):nrow(temp)),]
  temp$year <- paste0(year,"-",year+1)
  #colnames(temp)[3] <- "Publishing Dates for the  Final T-0 PLC Effective Price"
  temp <- temp[,which(colnames(temp) != "NA")] %>% na.omit() # omit NA
  if(year == 2014){
    colnames(temp) <- gsub(paste0(year,"/",substr(year + 1, 3,4),"|",year),"T-0",colnames(temp))
    colnames(temp) <- trimws(gsub("Projected|\\(P\\)|\\(F\\)| or ","",colnames(temp)))
    colnames(temp) <- trimws(gsub("  "," ",colnames(temp)))
  } else {
    colnames(temp) <- colnames(plc_payment_rates)
  }

  if(year > 2018){
    colnames(temp) <- gsub("Statutory","Effective",colnames(temp))
  }

  # if plc_payment_rates is null, define it
  if(is.null(plc_payment_rates)){
    plc_payment_rates <- temp
  } else {
    plc_payment_rates <- dplyr::bind_rows(plc_payment_rates, temp)
  }

}

# rename columns
colnames(plc_payment_rates) <- c("crop","marketing_year_dates",
                                 "publishing_dates_for_final_mya_price",
                                 "unit","statutory_reference_price",
                                 "current_mya_price","current_national_loan_rate",
                                 "plc_price","plc_payment_rate",
                                 "max_plc_payment_rate",
                                 "marketing_year","effective_reference_price")

# add program_year
plc_payment_rates$program_year <- substr(plc_payment_rates$marketing_year,1,4)

# type convert the columns
plc_payment_rates <- readr::type_convert(plc_payment_rates)


# add combined reference price
plc_payment_rates$combined_reference_price <-plc_payment_rates$statutory_reference_price
plc_payment_rates$combined_reference_price <- ifelse(is.na(plc_payment_rates$combined_reference_price),
                                                     plc_payment_rates$effective_reference_price,
                                                     plc_payment_rates$combined_reference_price)


# add rma codes
# extract crop type
plc_payment_rates$crop_type <- unlist(lapply(plc_payment_rates$crop, extract_crop_type))

# add rma crop codes where applicable
plc_payment_rates$rma_type_code <- unlist(lapply(plc_payment_rates$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
plc_payment_rates$crop <- unlist(lapply(plc_payment_rates$crop, clean_crop_names))

# add rma crop codes
plc_payment_rates$rma_crop_code <- unlist(lapply(plc_payment_rates$crop, assign_rma_cc))


# reorder columns
plc_payment_rates <- plc_payment_rates[,c(
  "crop",
  "marketing_year_dates",
  "marketing_year",
  "program_year",
  "publishing_dates_for_final_mya_price",
  "statutory_reference_price",
  "effective_reference_price",
  "combined_reference_price",
  "unit",
  "current_mya_price",
  "current_national_loan_rate",
  "plc_price",
  "plc_payment_rate",
  "max_plc_payment_rate",
  "crop_type",
  "rma_type_code",
  "rma_crop_code"
)]


# convert  data to a tibble before exporting
fsaPlcPaymentRate <- dplyr::as_tibble(plc_payment_rates)


# use the county level file in the package data folder
usethis::use_data(fsaPlcPaymentRate, overwrite = TRUE)


