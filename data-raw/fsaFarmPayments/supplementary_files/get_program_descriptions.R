
# get paths to all downloaded files
downloads <- list.files(path = "./data-raw/fsaFarmPayments/input_data", pattern = "*.xlsx", full.names = TRUE)

# load existing program details or create empty data frame if file doesn't exist
if(file.exists("./data-raw/fsaFarmPayments/supplementary_files/program_details.csv")) {
  prog_details <- readr::read_csv("./data-raw/fsaFarmPayments/supplementary_files/program_details.csv")
} else {
  prog_details <- data.frame(
    `Accounting Program Code` = character(0),
    `Accounting Program Description` = character(0),
    min_year = numeric(0),
    max_year = numeric(0),
    min_file_year = numeric(0),
    max_file_year = numeric(0),
    prog_abb = character(0),
    prog_full = character(0),
    stringsAsFactors = FALSE
  )
}

# create a temporary data frame to collect all program data
all_program_data <- data.frame(
  `Accounting Program Code` = character(0),
  `Accounting Program Description` = character(0),
  year = numeric(0),
  file_year = numeric(0),
  stringsAsFactors = FALSE
)

# cycle through each payment file to collect program codes, descriptions, and years
for(k in 1:length(downloads)){
  print(k) # print index to see progress of code
  n_rows <- 0 # reset row counter to 0
  sheet <- 1 # reset sheet counter to 1

  # extract year from filename (PMT followed by 2 digits) for file_year column
  filename <- basename(downloads[k])
  year_match <- regmatches(filename, regexpr("pmt\\d{2}", filename, ignore.case = TRUE))
  if(length(year_match) > 0) {
    year_digits <- gsub("pmt", "", year_match, ignore.case = TRUE)
    # convert 2-digit year to 4-digit year (assuming 20xx for years >= 00)
    file_year <- as.numeric(paste0("20", year_digits))
  } else {
    file_year <- NA
  }

   # this loop goes through each sheet in the excel file untill it finds one that is populated
  # this is neccesary, because some of the files have a blank sheet 1 and the data of interest is on sheet 2
  tryCatch({ # wrapping in a trycatch
    while(n_rows == 0){
      temp <- dplyr::distinct(readxl::read_excel(downloads[k],range = cellranger::cell_cols("N:P"), sheet = sheet))
      n_rows <- nrow(temp)
      sheet <- sheet + 1
      if(sheet > 10){
        break("Error: Couldn't find a populated sheet in the xlsx file.")
      }
    }
  } , error = function(e) { print("Problem reading .xlsx file")})

  # add data to all_program_data (year comes from Accounting Program Year column)
  if(nrow(temp) > 0) {
    colnames(temp)[3] <- "year"  # rename Accounting Program Year to year for consistency
    temp$file_year <- file_year  # add file year column
    all_program_data <- rbind(all_program_data, temp)
  }
}

# aggregate data to get unique program codes/descriptions with min and max years
aggregated_data <- all_program_data %>%
  dplyr::filter(year > 0 | is.na(year)) %>%  # exclude 0 years but keep NA
  dplyr::group_by(`Accounting Program Code`, `Accounting Program Description`) %>%
  dplyr::summarise(
    min_year = min(year[year >= 1000], na.rm = TRUE),  # only consider 4-digit years
    max_year = max(year, na.rm = TRUE),
    min_file_year = min(file_year[file_year >= 1000], na.rm = TRUE),  # only consider 4-digit years
    max_file_year = max(file_year, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(
    min_year = ifelse(is.infinite(min_year), NA, min_year),
    max_year = ifelse(is.infinite(max_year), NA, max_year),
    min_file_year = ifelse(is.infinite(min_file_year), NA, min_file_year),
    max_file_year = ifelse(is.infinite(max_file_year), NA, max_file_year)
  )

# merge with existing prog_details to preserve manual classifications
if(nrow(prog_details) > 0) {
  # update existing records with new year ranges
  prog_details <- prog_details %>%
    dplyr::left_join(aggregated_data, by = c("Accounting Program Code", "Accounting Program Description")) %>%
    dplyr::mutate(
      min_year = pmin(min_year.x, min_year.y, na.rm = TRUE),
      max_year = pmax(max_year.x, max_year.y, na.rm = TRUE),
      min_file_year = pmin(min_file_year.x, min_file_year.y, na.rm = TRUE),
      max_file_year = pmax(max_file_year.x, max_file_year.y, na.rm = TRUE)
    ) %>%
    dplyr::select(-min_year.x, -min_year.y, -max_year.x, -max_year.y,
                  -min_file_year.x, -min_file_year.y, -max_file_year.x, -max_file_year.y)

  # add completely new program codes/descriptions
  new_programs <- aggregated_data %>%
    dplyr::anti_join(prog_details, by = c("Accounting Program Code", "Accounting Program Description")) %>%
    dplyr::mutate(prog_abb = NA_character_, prog_full = NA_character_)

  prog_details <- rbind(prog_details, new_programs)
} else {
  # if prog_details is empty, use aggregated data as starting point
  prog_details <- aggregated_data %>%
    dplyr::mutate(prog_abb = NA_character_, prog_full = NA_character_)
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





