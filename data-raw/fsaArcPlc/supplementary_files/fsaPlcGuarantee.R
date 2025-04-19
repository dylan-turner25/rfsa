

#----------------------------------------
# PLC guarantee                       ####
load("./data/fsaPLCPaymentRate.rda")
fsaPLCPaymentRate <- as.data.frame(fsaPLCPaymentRate)[c("year","Commodity","Final T-0 Effective Price","Statutory Reference Price")]
names(fsaPLCPaymentRate) <- c("year","crop","price_final","price_reff")
fsaPLCPaymentRate$crop <- toupper(fsaPLCPaymentRate$crop)
fsaPLCPaymentRate$price_reff <- as.numeric(as.character(fsaPLCPaymentRate$price_reff))
fsaPLCPaymentRate$crop <- gsub("[/]"," ",fsaPLCPaymentRate$crop)
fsaPLCPaymentRate$crop <- gsub("  2","",fsaPLCPaymentRate$crop)
fsaPLCPaymentRate$crop <- gsub(" 2 ","",fsaPLCPaymentRate$crop)
fsaPLCPaymentRate$crop <- gsub(" 3 ","",fsaPLCPaymentRate$crop)


fsaPLCPaymentRate$crop <- ifelse(fsaPLCPaymentRate$crop %in% "RICE (TEMPORATE JAPONICA)","RICE (TEMPERATE JAPONICA)",fsaPLCPaymentRate$crop)
fsaPLCPaymentRate$cropx <- fsaPLCPaymentRate$crop
fsaPLCPaymentRate <- doBy::summaryBy(list("price_reff",c("year","cropx")),
                                     data=fsaPLCPaymentRate,FUN=mean,na.rm=T,keep.names = T)

data <- rbind(
  data.frame(readxl::read_excel("./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/2019_PLC_Ylds_Enrolled_Base.xlsx")),
  data.frame(readxl::read_excel("./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/2020_PLC_Ylds_Enrolled_Base.xlsx")),
  data.frame(readxl::read_excel("./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/2021_PLC_Ylds_Enrolled_Base.xlsx")),
  data.frame(readxl::read_excel("./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/plc_county_yields_py2022.xlsx")))

data$crop <- toupper(data$Crop.Name)
data$crop_yr <- data$Crop.Year
data$unit <- data$PLC_Yield_Units

data00 <- as.data.frame(
  readxl::read_excel("./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/ccp_county_yields_2018.xls", 
                     skip = 1))
data00 <- data00[2:nrow(data00),]
data00 <- data00 %>%  tidyr::gather(crop, PLC_Yield, 4:ncol(data00))
data00 <- dplyr::full_join(Counties,data00,by=c("ST_CTY"))
data00$state_cd <- as.numeric(as.character(data00$STATEFP))
data00$county_cd <- as.numeric(as.character(data00$COUNTYFP))
data00$PLC_Yield <- as.numeric(as.character(data00$PLC_Yield))
data00 <- doBy::summaryBy(list(c("PLC_Yield"),c("crop_yr","crop","state_cd","county_cd")),
                          data=data00,FUN=mean,na.rm=T,keep.names = T)
data00 <- data00[!data00$PLC_Yield %in% c(NA,NaN,Inf,-Inf,0),]
data00$crop <- toupper(data00$crop)

data00 <- future_lapply(
  2014:2018,
  function(crop_yr){
    # crop_yr <- 2019
    data <- data00
    data$crop_yr <- crop_yr
    return(data)
  })
data00[[length(data00)+1]] <- data
data <- as.data.frame(data.table::rbindlist(data00, fill = TRUE))
data <- data[!data$PLC_Yield %in% c(NA,NaN,Inf,-Inf,0),]

data$year <- ifelse(data$crop_yr %in% 2014,"2014-2015",
                    paste0(as.character(data$crop_yr-1),"-",as.character(data$crop_yr)))

table(data$crop,data$year)
table(fsaPLCPaymentRate$crop,fsaPLCPaymentRate$year)

# unique(data$cropx)[!unique(data$cropx) %in% unique(fsaPLCPaymentRate$crop)]
# unique(fsaPLCPaymentRate$crop)[!unique(fsaPLCPaymentRate$crop) %in% unique(data$cropx)]

data$cropx <- ifelse(data$crop %in% "RICE - LONG GRAIN","RICE (LONG GRAIN)",data$crop)
data$cropx <- ifelse(data$crop %in% "RICE-LONG GRAIN","RICE (LONG GRAIN)",data$cropx)
data$cropx <- ifelse(data$crop %in% "RICE - MED GRAIN","RICE (MED SHORT GRAIN)",data$cropx)
data$cropx <- ifelse(data$crop %in% "RICE-MED GRAIN","RICE (MED SHORT GRAIN)",data$cropx)
data$cropx <- ifelse(data$crop %in% "RICE-TEMP JAPONICA","RICE (TEMPERATE JAPONICA)",data$cropx)
data$cropx <- ifelse(data$crop %in% "SESAME","SESAME SEED",data$cropx)
data$cropx <- ifelse(data$crop %in% "SUNFLOWERS","SUNFLOWER SEED",data$cropx)
data$cropx <- ifelse(data$crop %in% "UPLAND COTTON LINT","SEED COTTON",data$cropx)
data$cropx <- ifelse(data$crop %in% "MUSTARD","MUSTARD SEED",data$cropx)

data <- dplyr::full_join(data,fsaPLCPaymentRate,by=c("year","cropx"))

data$guarantee <- data$PLC_Yield*data$price_reff
data$state_cd <- as.numeric(as.character(data$State))
data$county_cd <- as.numeric(as.character(data$County))
data <- doBy::summaryBy(list(c("price_reff","PLC_Yield","guarantee"),c("crop_yr","crop","state_cd","county_cd","unit")),
                        data=data,FUN=mean,na.rm=T,keep.names = T)
data$farm_guarantee <- data$guarantee*0.85
saveRDS(data,file="./data-raw/fsaArcPlc/Output/fsaPlcGuarantee.rds")

