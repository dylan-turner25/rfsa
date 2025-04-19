
# ARCCO Benchmarks -------------------------------------------------------

Counties <- rgdal::readOGR(dsn ="./data-raw/spatialData/Archive/usaPolygons", layer = "USA_Counties")@data
Counties$ST_CTY <- Counties$GEOID

data <- as.data.frame(
  readxl::read_excel(
    "./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arcco_benchmarks/arcco_benchmarks_2014-2018.xlsx", 
    col_names = FALSE, skip = 4))

varlist <- c("null_x_","yield_benchmark_x_","price_benchmark_x_","revnu_benchmark_x_","guarantee_x_",
             "guarantee_10_x_","yield_actual_x_","price_actual_x_","revnu_actual_x_","formula_rate_x_",
             "payment_rate_x_")

varlist <- c("ST_Cty","state","county","crop","unit","type",
             paste0("county_yield_",2009:2017),paste0(varlist,2014),paste0(varlist,2015),
             paste0(varlist,2016),paste0(varlist,2017),paste0(varlist,2018))

names(data) <- varlist

data <- data[names(data)[! grepl("null_x_",names(data))]]

data <- data %>%  tidyr::gather(variable, value, 
                                names(data)[! names(data) %in% c("ST_Cty","state","county","crop","unit","type",
                                                                 paste0("county_yield_",2009:2017))])

data <- tidyr::separate(data,"variable",into=c("variable","crop_yr"),sep="_x_")
data00 <- data %>%  tidyr::spread(variable, value)

data <- future_lapply(
  2019:2022,
  function(crop_yr){
    # crop_yr <- 2019
    print(crop_yr)
    data <- as.data.frame(
      readxl::read_excel(
        paste0("./data-raw/fsaArcPlc/Raw Data Files/arc_plc_Datadump/arcco_benchmarks/arcco_benchmarks_",crop_yr,".xlsx"), 
        col_names = FALSE, skip = ifelse(crop_yr %in% 2019 ,5,4)))
    
    if(crop_yr %in% 2019){
      varlist <- c("ST_Cty","state","county","crop","unit","type",
                   paste0("county_yield_",(crop_yr-6):(crop_yr-2)),"null","yield_benchmark",
                   "price_benchmark","revnu_benchmark","guarantee",
                   "guarantee_10","yield_actual","price_actual","revnu_actual","formula_rate",
                   "payment_rate")
    }else{
      varlist <- c("ST_Cty","state","county","county_sub","crop","unit","type",
                   paste0("county_yield_",(crop_yr-6):(crop_yr-2)),"null","yield_benchmark",
                   "price_benchmark","revnu_benchmark","guarantee",
                   "guarantee_10","yield_actual","price_actual","revnu_actual","formula_rate",
                   "payment_rate")
    }
    
    varlist <- varlist[1:ncol(data)]
    
    names(data) <- varlist
    data <- data[names(data)[! grepl("null",names(data))]]
    data$crop_yr <- crop_yr
    return(data)
  })

data[[length(data)+1]] <- data00

data <- as.data.frame(data.table::rbindlist(data, fill = TRUE))

data$ST_CTY <- data$ST_Cty

data <- dplyr::full_join(Counties,data,by=c("ST_CTY"))
data$state_cd <- as.numeric(as.character(data$STATEFP))
data$county_cd <- as.numeric(as.character(data$COUNTYFP))

county_yield <- names(data)[grepl("county_yield_",names(data))]
county_yield <- county_yield[order(as.numeric(gsub("[^0-9]","",county_yield)))]
data <- data[c("crop_yr","state_cd","county_cd","county_sub","crop","unit","type",county_yield,
               "yield_benchmark",
               "price_benchmark","revnu_benchmark","guarantee",
               "guarantee_10","yield_actual","price_actual","revnu_actual","formula_rate",
               "payment_rate")]
data$crop <- toupper(data$crop)
for(varb in c(county_yield,
              "yield_benchmark",
              "price_benchmark","revnu_benchmark","guarantee",
              "guarantee_10","yield_actual","price_actual","revnu_actual","formula_rate",
              "payment_rate")){
  data[,varb] <- as.numeric(as.character(data[,varb]))
}

data <- data[!data$guarantee %in% c(NA,NaN,Inf,-Inf,0),]
data$crop_yr <- as.numeric(as.character(data$crop_yr))
data$farm_guarantee <- data$guarantee*0.85

# drop if county code is not avaliable
data <- data[-which(is.na(data$county_cd)),]

# add fips variable
data$fips <- clean_fips(county = data$county_cd, state = data$state_cd)

# add crop year variable
data$crop_yr_full <- paste0(data$crop_yr,"-",data$crop_yr+1)


saveRDS(data,file="./data-raw/fsaArcPlc/Output/fsaArcCoBenchmarks.rds")

fsaArcCoBenchmarks <- tidyr::as_tibble(data)

# add data set to the data set folder
usethis::use_data(fsaArcCoBenchmarks, overwrite = TRUE)




