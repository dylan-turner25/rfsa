
# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaArcPlcPayments",
                    full.names = T, pattern = ".xlsx")

# initialize a data frame to store arc-ic prices
arc_plc_payments <- NULL

# load and combine county-commodity-year files for 2014-2018
for(year in 2014:2018){

  # get file path corresponding to year
  file <- files[grepl(year,files)]

  # load the file to a temporary object
  temp <- readxl::read_xlsx(file)

  if(grepl("ST_CTY",temp[1,1]) ){
    colnames(temp) <- temp[1,]
    temp <- temp[-1,]
    temp$`Amount Paid` <- as.numeric(temp$`Amount Paid`)
  }

  # add year
  temp$year <- year

  # group
  temp <- temp %>% group_by(Program,Crop,year) %>%
    summarize(`Amount Paid` = sum(`Amount Paid`, na.rm =T)) %>%
    na.omit()

  # if arc_plc_payments is null, define it
  if(is.null(arc_plc_payments)){
    arc_plc_payments <- temp
  } else {
    arc_plc_payments <- dplyr::bind_rows(arc_plc_payments, temp)
  }

}

# rename and reconcile different crop naming coventions
arc_plc_payments$Crop <- gsub("-","",arc_plc_payments$Crop)
arc_plc_payments$Crop <- str_to_title(arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Ckpeaslg","Large chickpeas",arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Ckpeassm","Small chickpeas",arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Ricemg","Rice-med grain",arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Ricelg","Rice-long grain",arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Safflwr","Safflower",arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Sunflr","Sunflower",arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Peasdry","Dry Peas",arc_plc_payments$Crop)
arc_plc_payments$Crop <- gsub("Sorghum","Grain sorghum",arc_plc_payments$Crop)

# replace na with 0
arc_plc_payments <- replace(arc_plc_payments, is.na(arc_plc_payments), 0)


# load and combine state-program files for 2019-2022
for(prog in c("ARCCO","ARCIC","PLC")){
  # get file path corresponding to year
  file <- files[grepl(prog,files)]

  # loop over each year, corresponding to a sheet in loaded excel file
  for(year in 2019:2023){
    # load the file to a temporary object
    temp <- readxl::read_xlsx(file, sheet = paste(year, prog), skip = 2)

    # remove grand total "state"
    temp <- temp[-which(temp$`STATE NAME` %in% c("GRAND TOTAL")),]

    # remove state and total columns
    temp <- temp[,-which(colnames(temp) %in% c("STATE NAME","TOTAL"))]

    # transpose aggregate to national level
    temp <- data.frame(t(temp))
    temp$`Amount Paid` <- rowSums(temp)
    temp$Crop <- rownames(temp)
    temp <- temp[,c("Crop","Amount Paid")]

    # add program and year
    temp$Program <- gsub("ARC","ARC-",prog)
    temp$year <- year

    # clean crop names
    temp$Crop <- gsub("Beans-","",temp$Crop)
    temp$Crop <- str_to_sentence(temp$Crop)

    # add to arc_plc_payments data frame
    arc_plc_payments <- dplyr::bind_rows(arc_plc_payments, temp)
  }

}

# type convert columns
arc_plc_payments <- readr::type_convert(arc_plc_payments)

# rename columns
names(arc_plc_payments) <- c("program","crop","program_year","payments")

# extract crop type
arc_plc_payments$crop_type <- unlist(lapply(arc_plc_payments$crop, extract_crop_type))

# add rma crop codes where applicable
arc_plc_payments$rma_type_code <- unlist(lapply(arc_plc_payments$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
arc_plc_payments$crop <- unlist(lapply(arc_plc_payments$crop, clean_crop_names))

# add rma crop codes
arc_plc_payments$rma_crop_code <- unlist(lapply(arc_plc_payments$crop, assign_rma_cc))



# save county-commodity-year payments
fsaArcPlcPayments <- arc_plc_payments

usethis::use_data(fsaArcPlcPayments, overwrite = TRUE)

