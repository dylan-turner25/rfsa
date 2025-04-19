# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/Raw Data Files/erp_prices",
                    full.names = T)

# initialize a data frame to store mya prices
erp_prices <- NULL


# load the files and bind them into one data frame
for(year in 2019:(current_year )){
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
  temp <- temp[-c(1:(commodity_row+1)),]
  
  # clean the raw data
  #colnames(temp) <- temp[3,]
  #temp <- temp[-c(1:4),]
  #temp <- temp[-c(which(is.na(temp$Commodity)):nrow(temp)),] 
  temp$year <- year
  temp$crop_year <- paste0(year - 1,"-",year)
  #colnames(temp)[3] <- "Publishing Dates for the  Final T-0 PLC Effective Price"
  temp <- temp[,which(colnames(temp) != "NA")] %>% na.omit() # omit NA
  if(year == 2019){
    colnames(temp) <- gsub(paste0(year,"/",substr(year + 1, 3,4),"|",year),"T-0",colnames(temp))
    colnames(temp) <- trimws(gsub("  "," ",colnames(temp)))
    for(y in c(13:22)){
      for(c in 1:ncol(temp)){
        if(grepl(paste0("20",y),colnames(temp)[c])){
          colnames(temp)[c] <- gsub(paste0("20",y,"/",y+1), 
                                    paste0("T-",(year-1 - as.numeric(paste0("20",y))  )), 
                                    colnames(temp)[c])
        }
      }
    }
  } else if (year >= 2023){
   colnames(temp) <- colnames(erp_prices)[-which(colnames(erp_prices) %in% c("MAX","MIN"))]
  } else {
    colnames(temp) <- colnames(erp_prices)
  }
  
  # if plc_payment_rates is null, define it
  if(is.null(erp_prices)){
   erp_prices <- temp
  } else {
    erp_prices <- dplyr::bind_rows(erp_prices, temp)
  }
  
}



# convert the fsaFarmPayments data to a tibble before exporting
fsaEffectiveRefPrices <- dplyr::as_tibble(erp_prices)


# use the county level file in the package data folder
usethis::use_data(fsaEffectiveRefPrices, overwrite = TRUE)


