
Counties <- rgdal::readOGR(dsn ="./data-raw/spatialData/Archive/usaPolygons", layer = "USA_Counties")@data
Counties$ST_CTY <- Counties$GEOID

# 2014 Farm Bill Election ------------------------------           
  data14 <- as.data.frame(
    readxl::read_excel(
      "data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/2014_FarmBill/2016 ARCPLC Enrolled Acre Report by Progam Sept 2017.xlsx", 
      sheet = "Sheet1"))
  
  data14 <- data14 %>%  tidyr::gather(crop, acres, 6:ncol(data14))
  data14$ST_CTY <- data14$`St.Cty Code`
  data14$prog <- ifelse(grepl("PLC ",data14$Program),"PLC",NA)
  data14$prog <- ifelse(grepl("ARC-IC ",data14$Program),"ARCIC",data14$prog)
  data14$prog <- ifelse(grepl("ARC-CO ",data14$Program),"ARCCO",data14$prog)
  data14$crop <- toupper(data14$crop)
  data14 <- doBy::summaryBy(acres~crop+ST_CTY+prog,data=data14,FUN=sum,keep.names = T,na.rm=T)
  
  Election14 <- as.data.frame(
    data.table::rbindlist(future_lapply(2014:2018,function(year){data <- data14;data$year <- year;return(data)
    }), fill = TRUE))
  rm(data14)


# 2018 Farm Bill Election ----------------------------------
  Election18 <- as.data.frame(
    data.table::rbindlist(
      future_lapply(
        2019:2022,
        function(year){
          #year <- 2021
          data1 <- as.data.frame(readxl::read_excel(
            paste0(farmpolicydata_path,"./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/",year,"_PLC_Ylds_Enrolled_Base.xlsx")
          ))
          data1$crop <- toupper(data1$`Crop Name`)
          data1$ST_CTY <- paste0(data1$State,data1$County)
          data1$TOTAL_PLC_ACRCO_ARCIC  <- data1$`ENROLLED BASE (PLC+ARC_CO+ARC_IC )`
          
          data1 <- dplyr::full_join(doBy::summaryBy(TOTAL_PLC_ACRCO_ARCIC~crop+ST_CTY,data=data1,FUN=sum,keep.names = T,na.rm=T),
                                    doBy::summaryBy(PLC_Yield~crop+ST_CTY,data=data1,FUN=mean,keep.names = T,na.rm=T),
                                    by=c("ST_CTY","crop"))
          
          data2 <- as.data.frame(readxl::read_excel(
            paste0(farmpolicydata_path,"./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arc_plc_election_data/",year,"_enrolled_base_county_crop_program.xlsx")
          ))
          data2$crop <- toupper(data2$`Crop Name`)
          data2$TOTAL_PLC_ACRCO  <- data2$`Enrolled Base`
          data2<- doBy::summaryBy(TOTAL_PLC_ACRCO~crop+ST_CTY+Program,data=data2,FUN=sum,keep.names = T,na.rm=T)
          
          
          data2$crop <- ifelse(data2$crop %in% "CHICKPEAS_LARGE","LARGE CHICKPEAS",data2$crop)
          data2$crop <- ifelse(data2$crop %in% "CHICKPEAS_SMALL","SMALL CHICKPEAS",data2$crop)
          data2$crop <- ifelse(data2$crop %in% "RICE_LONG GRAIN","RICE-LONG GRAIN",data2$crop)
          data2$crop <- ifelse(data2$crop %in% "RICE_MED/SHORT GRAIN","RICE-MED GRAIN",data2$crop)
          data2$crop <- ifelse(data2$crop %in% "RICE_TEMPERATE JAPONICA","RICE-TEMP JAPONICA",data2$crop)
          data2$crop <- ifelse(data2$crop %in% "MUSTARD SEED","MUSTARD",data2$crop)
          data2$crop <- ifelse(data2$crop %in% "SUNFLOWER SEED","SUNFLOWERS",data2$crop)
          data2$crop <- ifelse(data2$crop %in% "SESAME SEED","SESAME",data2$crop)
          
          # unique(data1$crop)[!unique(data1$crop) %in% unique(data2$crop)]
          # unique(data2$crop)[!unique(data2$crop) %in% unique(data1$crop)]
          
          
          data <- dplyr::full_join(doBy::summaryBy(TOTAL_PLC_ACRCO~crop+ST_CTY,data=data2,FUN=sum,keep.names = T,na.rm=T),data1,by=c("ST_CTY","crop"))
          
          data$ARCIC <- data$TOTAL_PLC_ACRCO_ARCIC-data$TOTAL_PLC_ACRCO
          data$ARCIC <- ifelse(data$ARCIC<0,0,data$ARCIC)
          data <- doBy::summaryBy(ARCIC+PLC_Yield~crop+ST_CTY,data=data,FUN=sum,keep.names = T,na.rm=T)
          
          data2 <- data2[c("ST_CTY","crop","Program","TOTAL_PLC_ACRCO")] %>%  tidyr::spread(Program, TOTAL_PLC_ACRCO) 
          
          data <- dplyr::full_join(data2,data,by=c("ST_CTY","crop"))
          data$year <- year
          return(data)
        }), fill = TRUE))
  Election18 <- Election18[c("ST_CTY","crop","PLC_Yield" ,"year","ARCCO","PLC","ARCIC")]
  Election18 <- Election18 %>%  tidyr::gather(prog, acres, 5:ncol(Election18))

  
# cleanup election data -----------------------------------
Election <- as.data.frame(data.table::rbindlist(list(Election14,Election18), fill = TRUE))

Election <- dplyr::full_join(Counties,Election,by=c("ST_CTY"))

Election <- Election[!Election$prog %in% NA,c("year","STATEFP","COUNTYFP","crop","prog","acres","PLC_Yield")]
names(Election) <- c("crop_yr","state_cd","county_cd","crop","prog","acres","PLC_Yield")

Election$state_cd <- as.numeric(as.character(Election$state_cd))
Election$county_cd <- as.numeric(as.character(Election$county_cd))
Election$acres <- as.numeric(as.character(Election$acres))
Election$PLC_Yield <- as.numeric(as.character(Election$PLC_Yield))

saveRDS(Election,file="./data-raw/fsaArcPlc/Output/fsaArcPlcCountyCropElection.rds")





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
saveRDS(data,file="./data-raw/fsaArcPlc/Output/fsaPLCCOBenchmarks.rds")
#----------------------------------------


