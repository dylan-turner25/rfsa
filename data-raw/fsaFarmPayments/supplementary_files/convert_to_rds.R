library(tidyverse)
library(future)
library(future.apply)

# get paths to each individual rds files and downloaded xlsx files
downloads <- list.files(path = "./data-raw/fsaFarmPayments/input_data", pattern = "*.xlsx", full.names = TRUE)
rds_files <- list.files(path = "./data-raw/fsaFarmPayments/input_data", pattern = "*.rds", full.names = TRUE)

# convert the excel files to rds files without applying any cleaning
for(i in 1:length(downloads)){
  print(i)

  # skip if there is already an rds version of the file
  if(!(gsub(".xlsx",".rds",downloads[i]) %in% rds_files)){

    n_rows <- 0 # reset row counter to 0
    sheet <- 1 # reset sheet counter to 1
    redownload <- 1 # defaults to re-downloading the .xlsx file, if script is able to read the file, this gets turned off (i.e. set to t0)

    # this loop goes through each sheet in the excel file until it finds one that is populated
    # this is necessary, because some of the files have a blank sheet 1 and the data of interest is on sheet 2
    tryCatch({ # wrapping in a trycatch
      while(n_rows == 0){
        temp <- readxl::read_excel(downloads[i], sheet = sheet) # load next xslx file in the directory
        n_rows <- nrow(temp)
        sheet <- sheet + 1
        if(sheet > 10){
          break("Error: Couldn't find a populated sheet in the xlsx file.")
        }
      }
      redownload <- 0 # let next section of code know the file was successfully read and not to try redownloading the file.
    } , error = function(e) { print("Problem reading .xlsx file, script will redownload the file and try reading it again") } )

    # export rds
    saveRDS(temp, gsub(".xlsx",".rds",downloads[i]) )
  }
}
