
# get paths to all downloaded files
downloads <- list.files(path = "./data-raw/fsaFarmPayments/input_data", pattern = "*.xlsx", full.names = TRUE)

# load existing program details
prog_details <- readr::read_csv("./data-raw/fsaFarmPayments/supplementary_files/program_details.csv")

# cycle through each payment file to check for new program codes or descriptions,
# if there is a new one, it will be initiallized in  the prog_details data set, but
# will have to be manually updated
for(k in 1:length(downloads)){
  print(k) # print index to see progress of code
  n_rows <- 0 # reset row counter to 0
  sheet <- 1 # reset sheet counter to 1

   # this loop goes through each sheet in the excel file untill it finds one that is populated
  # this is neccesary, because some of the files have a blank sheet 1 and the data of interest is on sheet 2
  tryCatch({ # wrapping in a trycatch
    while(n_rows == 0){
      temp <- distinct(readxl::read_excel(downloads[k],range = cellranger::cell_cols("N:O"), sheet = sheet))
      n_rows <- nrow(temp)
      sheet <- sheet + 1
      if(sheet > 10){
        break("Error: Couldn't find a populated sheet in the xlsx file.")
      }
    }
  } , error = function(e) { print("Problem reading .xlsx file")})

  if(F %in% c(temp$`Accounting Program Code` %in% prog_details$`Accounting Program Code`)){
    missing <- temp %>% filter(F == c(temp$`Accounting Program Code` %in% prog_details$`Accounting Program Code`))
    to_add <- data.frame(matrix(nrow = nrow(missing), ncol = ncol(prog_details)))
    colnames(to_add) <- colnames(prog_details)
    to_add$`Accounting Program Code` <- missing$`Accounting Program Code`
    to_add$`Accounting Program Description` <- missing$`Accounting Program Description`
    prog_details <- rbind.data.frame(to_add, prog_details)
  }

  if(F %in% c(temp$`Accounting Program Description` %in% prog_details$`Accounting Program Description`)){
    missing <- temp %>% filter(F == c(temp$`Accounting Program Description` %in% prog_details$`Accounting Program Description`))
    to_add <- data.frame(matrix(nrow = nrow(missing), ncol = ncol(prog_details)))
    colnames(to_add) <- colnames(prog_details)
    to_add$`Accounting Program Code` <- missing$`Accounting Program Code`
    to_add$`Accounting Program Description` <- missing$`Accounting Program Description`
    prog_details <- rbind.data.frame(to_add, prog_details)
  }
}

# load  prog_details
# If error message suggests updating "program_detail_manual.csv", do so then re run "fsaProgramClassifications.R" again to recreated the "fsaProgramDetails" data set.
missing_data_count <- nrow(prog_details[is.na(prog_details$prog_abb),])
if(missing_data_count > 0){

  readr::write_csv(prog_details,"./data-raw/fsaFarmPayments/supplementary_files/program_details.csv")

  # stop the script and let the user know that they need to update the program descriptions
  stop(paste0(missing_data_count, " observations in program_details.csv have missing values.
              These need to be filled in before continuing by manually opening program_detail_manual.csv
              in Microsoft Excel and classifying the programs associated with each program that have missing data.
              Failing to do so will result in aggregated statistics that are not accurate."))
}





