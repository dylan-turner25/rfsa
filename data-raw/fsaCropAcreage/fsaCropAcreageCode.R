
# clear all output
rm(list=ls(all=T))

# load libraries
library(tidyverse)
library(arrow)

# set working directory
setwd("./data-raw/fsaCropAcreage")

# create sub-directories if they don't already exists
dir.create(paste0("./Cleaned_Data")) # to store cleaned data
dir.create(paste0("./Raw_Data")) # to store raw data

# set timeout (i.e. howlong R will try to download a file before aborting)
options(timeout = max(300, getOption("timeout")))

# define years to get data for
start_year <- 2014
end_year <- as.numeric(substr(Sys.Date(),1,4)) # automatically set to current year

# load excel file with acreage urls
acreage_urls <- readxl::read_xlsx("./fsa_acreage_urls.xlsx")

# convert file release dates to `date` format
acreage_urls$date <- as.Date(acreage_urls$date, origin = "1970-01-01")

# remove any missing observations (there should't be any)
acreage_urls <- acreage_urls %>% na.omit()

# fix old urls to be in the format of new FSA website structure
acreage_urls$url <- gsub("/Assets/USDA-FSA-Public/usdafiles/NewsRoom/eFOIA/crop-acre-data/zips/+[0-9]+-crop-acre-data/","https://www.fsa.usda.gov/sites/default/files/documents/",acreage_urls$url)

# add prefix to urls (this can be updated if it ever chagnes)
#acreage_urls$url <- paste0("https://www.fsa.usda.gov",acreage_urls$url)

# remove urls that aren't in the range of start_year to end_year
acreage_urls <- acreage_urls %>% filter(year >= start_year, year <= end_year)


# download the raw data using the urls from the loaded acreage url file

# loop over each element of the above list and download the data
for(i in 1:nrow(acreage_urls)){
  # if there is already a file in the `Raw_Data` file corresponding to the url, skip and don't download
  if(! gsub("-","_",paste0("FSA_Acre_",acreage_urls$date[i],".zip")) %in% list.files(path="./Raw_Data")){
    download.file(paste0(acreage_urls$url[i]),destfile=paste0("./Raw_Data/","FSA_Acre_",gsub("-","_",acreage_urls$date[i]),".zip"),mode="wb")
  }
}


# the download files are .zip files and need to be extracted
acreage_urls$file_name <- NA
for(i in 1:nrow(acreage_urls)){

  # if a corresponding clean file already exists for the zip file, skip it
  if(!paste0("fsa_acres_",gsub("-","_",acreage_urls$date[i]),".rds") %in% list.files("./Cleaned_Data")){

    # set up a temporary directory to extract the .zip files to
    zip_files <- tempdir()

    zip_return <-  unzip(paste0("./Raw_Data/FSA_Acre_",
                   gsub("-","_",acreage_urls$date[i]),".zip"),
            exdir=zip_files,overwrite=T)

    # store the location of the temporary file in the acreage urls data frame
    acreage_urls$file_name[i] <- zip_return

  }

}

# download the intended use codes
# Not necessary, but may be helpful to reference.
# can be wrapped in a try statement since failure isn't critical
try({
  download.file("https://www.fsa.usda.gov/Assets/USDA-FSA-Public/usdafiles/NewsRoom/eFOIA/crop-acre-data/pdfs/intended_use_codes.pdf",
                destfile=paste0("./Raw_Data/","intended_use_codes.pdf"),mode="wb")
})


# loop over each release date and load and clean the corresponding file
for(i in unique(acreage_urls$date)){

  # if a corresponding clean file already exists, skip
  if(!(paste0("fsa_acres_",gsub("-","_",as.Date(i)),".rds") %in% list.files("./Cleaned_Data"))){

    # load the meta data (i.e. the data stored in the acreage_urls file)
    meta_data <- acreage_urls[which(acreage_urls$date == i),]
    if(nrow(meta_data) > 1){
      stop("meta_data has more than 1 row")
    }

    # extract the year (corresponding to the acreage data, not the file release year)
    year <- meta_data$year
    if(meta_data$month == 1){
      year <- year - 1 # adjust to previous year if month is january
    }

    # load the acreage file
    acres <- as.data.frame(readxl::read_excel(meta_data$file_name, sheet = "county_data"))

    # check to make sure colnames are not in the first row, load again skipping first row
    if(grepl("state", tolower(acres[1,1]))){
      acres <- as.data.frame(readxl::read_excel(meta_data$file_name, sheet = "county_data", skip = 1))
    }

    # clean the column names
    names(acres) <- gsub("state_fsa_code","State Code",names(acres))
    names(acres) <- gsub("county_fsa_code","County Code",names(acres))
    names(acres) <- tolower(gsub(" ","_",names(acres)))
    names(acres) <- gsub("crop_codes","crop_code",names(acres))
    names(acres) <- gsub("state_county_code","fips",names(acres))

    # clean the state and county codes
    acres$state_cd <- stringr::str_pad(as.numeric(as.character(acres$state_code)),2,pad="0")
    acres$county_cd <- stringr::str_pad(as.numeric(as.character(acres$county_code)),3,pad="0")

    # record the crop year
    acres$crop_yr <- year

    # record the release date
    acres$release_date <- meta_data$date
    acres$release_month <- meta_data$month
    acres$release_year <- meta_data$year
    acres$release_day <- meta_data$day

    # enforce column data types
    for(c in c("state_code","county_code","crop_code","fips","planted_acres",
               "volunteer_acres","failed_acres","prevented_acres",
               "not_planted_acres",
               "planted_and_failed_acres","state_cd","county_cd","crop_yr",
               "release_month","release_year","release_day")){
      acres[,c] <- as.numeric(acres[,c])
    }
    acres$release_date <- as.Date(acres$release_date)

    # save the cleaned file
    saveRDS(acres,paste0("./Cleaned_Data","/fsa_acres_",gsub("-","_",meta_data$date),".rds"))
    }

   message("Cleaning acreage file released on: ", as.Date(i))

  }


# combine into a single fsaCropAcreage dataset
fsaCropAcreage <- NULL
fsaCropAcreage <- list.files(paste0("./Cleaned_Data"),
                             pattern = ".rds",
                             full.names = T) %>%
  map(readRDS) %>%
  bind_rows()


# apply any final cleaning operations

# enforce common column naming conventions
colnames(fsaCropAcreage) <- gsub("_code","_cd",colnames(fsaCropAcreage))
colnames(fsaCropAcreage) <- gsub("_year","_yr",colnames(fsaCropAcreage))

# remove duplicated column names
fsaCropAcreage <- fsaCropAcreage[,!duplicated(colnames(fsaCropAcreage))]

# convert columns to numeric where appropriate
for(k in seq_len(ncol(fsaCropAcreage))){

  if(!"Date" %in% class(fsaCropAcreage[,k])){

  # check to see how many NA values are in the column
  old_na_count <- sum(is.na(fsaCropAcreage[,k]))

  # convert the column to numeric
  temp <- as.numeric(fsaCropAcreage[,k])

  # check to see how many NA values there are after conversion to numeric
  new_na_count <- sum(is.na(temp))

  test <-  data.frame(is.na(fsaCropAcreage[,k]),is.na(temp))

  # check to ensure the new_na_count is not greater than the old_na_count
  # which represents a loss of information during conversion. If there was
  # no loss of information, proceed with storing the converted column in the
  # fsaCropAcreage data frame
  if(new_na_count == old_na_count){
    fsaCropAcreage[,k] <- temp
  }

  }
}

# create a current release column
dates <- distinct(fsaCropAcreage %>% select(crop_yr, release_date)) %>%
  group_by(crop_yr) %>%
  summarize(max_date = max(release_date))

fsaCropAcreage$current_release <- F
fsaCropAcreage$current_release[which(fsaCropAcreage$release_date %in% dates$max_date)] <- T


fsaCropAcreage <- fsaCropAcreage %>% filter(current_release == T)

# rename crop type fsa_crop_type
colnames(fsaCropAcreage)[which(colnames(fsaCropAcreage) == "crop_type" )] <- "fsa_crop_type"

# extract crop type from crop name
fsaCropAcreage$crop_type <- unlist(lapply(fsaCropAcreage$crop, extract_crop_type))

# extract crop type from fsa_crop_type
fsaCropAcreage$crop_type2 <- unlist(lapply(fsaCropAcreage$fsa_crop_type, extract_crop_type))
fsaCropAcreage$crop_type[which(is.na(fsaCropAcreage$crop_type))] <- fsaCropAcreage$crop_type2[which(is.na(fsaCropAcreage$crop_type))]
fsaCropAcreage <- fsaCropAcreage %>% select(-crop_type2)


# add rma crop codes where applicable
fsaCropAcreage$rma_type_code <- unlist(lapply(fsaCropAcreage$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
fsaCropAcreage$crop <- unlist(lapply(fsaCropAcreage$crop, clean_crop_names2))

# add rma crop codes
fsaCropAcreage$rma_crop_code <- unlist(lapply(fsaCropAcreage$crop, assign_rma_cc))


# convert the data to a tibble
fsaCropAcreage <- dplyr::as_tibble(fsaCropAcreage)

# use the aggregated file in the package data folder
saveRDS(fsaCropAcreage, file = "./fsaCropAcreageData.rds")

# also export as parquet file
#arrow::write_parquet(fsaCropAcreage, "./fsaCropAcreageData.parquet")

# use the county level file in the package data folder
usethis::use_data(fsaCropAcreage, overwrite = TRUE)










