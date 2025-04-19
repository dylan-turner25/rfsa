# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/Raw Data Files/arc_plc_base_acres",
                    full.names = T)

# initialize a data frame to store arc-ic prices
arc_plc_base_acres <- NULL

for(year in 2015:(current_year - 1) ){
  # get file path corresponding to year
  file <- files[grepl(year,files)]
  
  if(year < 2018){
    sheet <- paste0(year, " Enrolled_Attributed")
    #try(temp <- readxl::read_xls(file, sheet = sheet))
    try(temp <- readxl::read_excel(file, sheet = sheet))
    #temp <- readxl::read_excel(file, sheet = sheet)
    
    sheet = "2017 Enrolled Base_AttGB"
    #try(temp <- readxl::read_xls(file, sheet = sheet))
    try(temp <- readxl::read_excel(file, sheet = sheet))

    temp <- data.frame(temp)
    
    colnames1 <- temp[which(grepl("Covered", temp[,1])),]
    for(k in seq_len(length(colnames1))){
      if(is.na(colnames1[k])){
        colnames1[k] <- colnames1[k-1]
      }
    }

    colnames1 <- replace(colnames1, grepl("ARC-CO",colnames1),"ARC-CO")
    colnames1 <- replace(colnames1, grepl("PLC",colnames1),"PLC")
    colnames1 <- replace(colnames1, grepl("ARC-IC",colnames1),"ARC-IC Enrolled Base")
    colnames1 <- replace(colnames1, grepl("Total",colnames1),"Total")
    colnames2 <- temp[which(grepl("Covered", temp[,1])) + 1,]
    colnames2[is.na(nchar(colnames2))] <- ""
    
    colnames <- t(rbind.data.frame(colnames1, colnames2))
    colnames <- trimws(paste(colnames[,1], colnames[,2]))
    
    colnames(temp) <- colnames
    
    temp <- temp[-c(1:which(grepl("Commodity", temp[,1]))),]
    
    temp <- temp[-c(which(is.na(temp$Total))),]
    
    for(k in 2:ncol(temp)){
      temp[,k] <- as.numeric(temp[,k])*1000
    }
    colnames(temp) <- gsub("Base Acres","Base",colnames(temp))
    
    temp[,"PLC Total"] <- temp$`PLC Covered Commodity Contract Base`+temp$`PLC Plantings  Attributed to Generic Base`
    temp[,"ARC-CO Total"] <- temp$`ARC-CO Covered Commodity Contract Base`+temp$`ARC-CO Plantings  Attributed to Generic Base`
    temp[,"ARC-IC Total"] <- temp$`ARC-IC Enrolled Base Covered Commodity Contract Base`
    

    colnames(temp) <- tolower(colnames(temp))
    
  }
  
  if(year >= 2018){
    temp <- readxl::read_excel(file)

    temp <- data.frame(temp)
    
    colnames1 <- temp[which(grepl("Covered", temp[,1])),]
    colnames1[is.na(nchar(colnames1))] <- "Agriculture Risk Coverage - County (ARC-CO)"
    colnames1 <- replace(colnames1, grepl("ARC-CO",colnames1),"ARC-CO")
    colnames1 <- replace(colnames1, grepl("PLC",colnames1),"PLC")
    colnames1 <- replace(colnames1, grepl("ARC-IC",colnames1),"ARC-IC")
    colnames1 <- replace(colnames1, grepl("Total",colnames1),"Total")
    
    colnames2 <- temp[which(grepl("Covered", temp[,1])) + 1,]
    colnames2[is.na(nchar(colnames2))] <- ""
    
    colnames <- t(rbind.data.frame(colnames1, colnames2))
    colnames <- trimws(paste(colnames[,1], colnames[,2]))
    
    colnames(temp) <- colnames
    
    temp <- temp[-c(1:which(grepl("Commodity", temp[,1]))),]
    
    if(is.na(temp[1,1])){
      temp <- temp[-1,]
    }
    
    colnames(temp) <- gsub("Enrolled Base Acres","Total", colnames(temp))
    
    to_drop <- which(grepl("%",colnames(temp)))
    if(length(to_drop > 0)){
      temp <- temp[,-to_drop]
    }

    for(k in 2:ncol(temp)){
      temp[,k] <- as.numeric(temp[,k])*(1+as.numeric(year == 2018)*999)
      
      if(colnames(temp)[k] %in% c("PLC","ARC-IC")){
        colnames(temp)[k] <- paste0(colnames(temp)[k]," Total")
      }
    }
    
    colnames(temp) <- gsub("Total Base Acres","total",colnames(temp))

    colnames(temp) <- tolower(colnames(temp))
    
    colnames(temp) <- gsub("non-irrigated","nonirrigated",colnames(temp))
    

    
  }
  
  temp$year <- year
  if( year > 2015 & year != 2018 & F %in% c(colnames(temp) %in% colnames(arc_plc_base_acres))){
    stop()
  }
  arc_plc_base_acres <- dplyr::bind_rows(arc_plc_base_acres, temp)
}

arc_plc_base_acres <- arc_plc_base_acres[grepl("total",
                                               tolower(arc_plc_base_acres$`covered commodity`)) == F,]

# convert to a tibble before exporting
fsaArcPlcBaseAcres <- dplyr::as_tibble(arc_plc_base_acres)

# use the county level file in the package data folder
usethis::use_data(fsaArcPlcBaseAcres, overwrite = TRUE)



