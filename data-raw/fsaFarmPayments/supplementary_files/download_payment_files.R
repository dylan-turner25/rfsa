
# identify opperating system
system <- tolower(Sys.info())
if(T %in% grepl("windows",system)){
  os <-"windows"
}
if(T %in% grepl("linux",system)){
  os <-"linux"
}
if(T %in% grepl("darwin",system)){
  os <-"mac"
}
## need to get access to a mac to add option for mac os

# set timeout
options(timeout=1000)

# scrape all text off FSA webpage to get list of all avaliable files
url <- "https://www.fsa.usda.gov/tools/informational/freedom-information-act-foia/electronic-reading-room/frequently-requested/payment-files"
html <- paste(readLines(url), collapse="\n") # capture all the html code
links <- as.character(data.frame(str_match_all(html, "href=\"(.*?)\""))[,1]) # locate and extract all links on the page
files <- links[grepl(".xlsx",links)] # pickout only the links that lead to excel files


# manual url entries: 2024 files introduced a nested structure that is difficult to scrape,
# adding he  4 or 5 urls mannually each year is easier
manual_additions <- c(
  "2025-01/state%20ia-mi.foia_.na_.pmt24.final_.dt25002.xlsx",
  "2025-01/state%20tx-wy.foia_.na_.pmt24.final_.dt25002.xlsx",
  "2025-01/state%20nd-tn.foia_.na_.pmt24.final_.dt25002.xlsx",
  "2025-01/state%20mn-nc.foia_.na_.pmt24.final_.dt25002.xlsx",
  "2025-01/state%20al-in.foia_.na_.pmt24.final_.dt25002.xlsx"
)

files <- c(files,manual_additions)

# clean up the files links
files <- gsub("href=\"/sites/default/files/documents/","",files, fixed = T) # remove non-unique prefix from file links
files <- gsub("xlsx\"","xlsx",files) # remove non-unique prefix from file links
#files <- gsub("2023/2018-payments-files/","",files) # remove non-unique prefix from file links
#for(y in 2000:2030){
#  files <- gsub(paste0("2023/",y,"-payment-files/"),"",files)
#  files <- gsub(paste0(y,"-"),"",files)
#  files <- gsub(paste0(y),"",files)
#}
#files <- gsub("\\/","",files)


xlsx_location <- str_locate(files,"xlsx") # locate where .xlsx is in the string
files <- substr(files,1,xlsx_location[,2])  # subset file links to end at .xlsx

existing_files <- list.files("./data-raw/fsaFarmPayments/input_data")

# loop through all the files links and download each one to the working directory
for(i in 1:length(files)){
  print(i)
  #url_temp <-paste0("https://www.fsa.usda.gov/Assets/USDA-FSA-Public/usdafiles/NewsRoom/eFOIA/payment-files/excel/",files[i])
  url_temp <-paste0("https://www.fsa.usda.gov/sites/default/files/documents/",files[i])
  file_name <- trimws(gsub("2025-01/"," ",files[i]))#substr(files[i],str_locate(files[i],"/")[1]+1,nchar(files[i]))

  # only download the files if it isn't already in the directory
  if(file_name %in% existing_files == F){
    if(os == "windows"){
      download.file(url_temp, destfile = paste0("./data-raw/fsaFarmPayments/input_data/",file_name), mode = "wb") # for windows machines
    }
    if(os == "linux"){
      download.file(url_temp, destfile =paste0("./data-raw/fsaFarmPayments/input_data/",file_name), method = "curl", extra = "-k") # for linux machines
    }
    if(os == "mac"){
      Sys.setenv(CURL_HTTP_VERSION = "1.1")
      try({
      download.file(url_temp, destfile = paste0("./data-raw/fsaFarmPayments/input_data/",file_name), method = "libcurl")
      })
    }
  }
}


# fsa_payment <- tempfile(fileext = ".xlsx")
# download.file(paste0(PaymentFiles$url[i]),destfile=fsa_payment,mode="wb")
# sheet <- readxl::excel_sheets(path = fsa_payment)





