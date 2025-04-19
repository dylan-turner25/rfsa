# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/Raw Data Files/plc_payment_rates",
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
  
  # if plc_payment_rates is null, define it
  if(is.null(plc_payment_rates)){
    plc_payment_rates <- temp
  } else {
    plc_payment_rates <- dplyr::bind_rows(plc_payment_rates, temp)
  }
  
}



# convert the fsaFarmPayments data to a tibble before exporting
fsaPLCPaymentRate <- dplyr::as_tibble(plc_payment_rates)


# use the county level file in the package data folder
usethis::use_data(fsaPLCPaymentRate, overwrite = TRUE)


