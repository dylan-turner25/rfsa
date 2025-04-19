# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/arc_ic_prices",
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


# strip any number followed by a "/" from the commodity name
arc_ic_prices$Commodity <- gsub("[0-9]/","",arc_ic_prices$Commodity)

# type convert the columns
arc_ic_prices <- readr::type_convert(arc_ic_prices)


# convert the fsaFarmPayments data to a tibble before exporting
fsaArcIcPrice <- dplyr::as_tibble(arc_ic_prices)

# use the county level file in the package data folder
usethis::use_data(fsaArcIcPrice, overwrite = TRUE)

