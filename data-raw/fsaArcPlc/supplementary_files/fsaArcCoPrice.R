files <- list.files("./data-raw/fsaArcPlc/input_data/arc_co_prices",
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

# convert the fsaFarmPayments data to a tibble before exporting
fsaArcCoPrice <- dplyr::as_tibble(arc_co_prices)

# type check columns
fsaArcCoPrice <- readr::type_convert(fsaArcCoPrice)

# remove any footnote indictors in commodity names
fsaArcCoPrice$Commodity <- trimws(gsub("[0-9]/","",fsaArcCoPrice$Commodity))

# use export the data to the the package data folder
usethis::use_data(fsaArcCoPrice, overwrite = TRUE)


