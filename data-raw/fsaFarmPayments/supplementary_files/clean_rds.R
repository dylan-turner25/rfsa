
# get vector of all the RDS files
rds_files <- list.files(path = "./data-raw/fsaFarmPayments/input_data", pattern = "*.rds", full.names = TRUE)

# remove rds files that are already cleaned
rds_files <- rds_files[!grepl("cleaned",rds_files)]

# make sure the number rds files matches the number of downloads
if(length(rds_files) != length(downloads)){
  stop("The number of rds files does not match the number of raw excel files.")
}

# load the first rds file and set up a column name template
{
  temp <-readRDS(rds_files[1])

  # clean names using janitor
  temp <- janitor::clean_names(temp)

  # add a payment year variable
  temp$payment_year <- substr(temp$payment_date,1,4) # add a payment year variable

  # add a fiscal year variable
  temp$fiscal_year <- sapply(as.Date(temp$payment_date),get_fiscal_year) # create a fiscal year variable

  # add detail info on fsa programs
  temp <- dplyr::left_join(temp, janitor::clean_names(program_details) %>%
                             dplyr::select(accounting_program_code,
                                           accounting_program_description,
                                           prog_abb, prog_full)) # merge program detail

  # add fips code
  temp$fips <- clean_fips(state = temp$state_fsa_code, county = temp$county_fsa_code)


  # remove "..." and number from column names
  #colnames(temp) <- gsub("\\.+\\d+","",colnames(temp))

  # initialize a df to hold column names of each file
  # (will use this to check consistency of column names accross files)
  columns <- data.frame(colnames(temp))
  colnames(columns) <- "col1"
  columns$col1 <- as.character(columns$col1)
  col_names <- colnames(temp) # column names of existing file
}

# create a template
template <- data.frame(matrix(nrow = 0, ncol = length(colnames(temp))))
colnames(template) <- colnames(temp)

# load in each individual payment file, clean it, and save it with a "cleaned"
# appendix
for(i in 1:length(rds_files)){
  print(i) # monitor loop progress
  # name of the file
  file_name <-  rds_files[i]

  # get vector of already cleaned files
  cleaned_files <- list.files(path = "./data-raw/fsaFarmPayments/input_data", pattern = "cleaned.rds", full.names = TRUE)

  # skip if a cleaned version of the file is already in the folder
  if(!(gsub(".rds","_cleaned.rds",file_name) %in% cleaned_files )){

    temp <-readRDS(rds_files[i])

    # clean names using janitor
    temp <- janitor::clean_names(temp)

    # add a payment year variable
    temp$payment_year <- substr(temp$payment_date,1,4) # add a payment year variable

    # add a fiscal year variable
    temp$fiscal_year <- sapply(as.Date(temp$payment_date),get_fiscal_year) # create a fiscal year variable

    # add detail info on fsa programs
    temp <- dplyr::left_join(temp, janitor::clean_names(program_details) %>%
                               dplyr::select(accounting_program_code,
                                             accounting_program_description,
                                             prog_abb, prog_full)) # merge program detail

    # add fips code
    temp$fips <- clean_fips(state = temp$state_fsa_code, county = temp$county_fsa_code)


    # add column names of temp to "columns" to check and makes sure columns all match across files
    columns[,ncol(columns)+1] <- colnames(temp)[1:nrow(columns)]
    colnames(columns)[ncol(columns)] <- paste0("col",ncol(columns))

    # check existing files to see if rows match
    unique_namesets <- nrow(unique(t(  data.frame(columns[,1], colnames(temp))))) # make sure strings in first row all match

    # algorithm to determine if the column differences are innocuous

    # start by assuming we can't ignore the difference
    ignore_diff <- F

    # if there was a difference detected, investigate
    if(unique_namesets > 1){
      # identify index of problem cases
      problem <- which(c(columns[,1] == colnames(temp)) == F)
      safe_to_ignore <- c()
      for(p in 1:length(problem)){
        n1 <- gsub(" ","",tolower(columns[p,1]))
        n2 <-  gsub(" ","",tolower(colnames(temp)[p]))
        if(grepl(n1,n2) | grepl(n2,n1)){
          safe_to_ignore <- c(safe_to_ignore,TRUE)
        } else {
          safe_to_ignore <- c(safe_to_ignore,FALSE)
        }
      }
      if(!(FALSE %in% safe_to_ignore)){
        ignore_diff <- TRUE
      }
    }

    if(unique_namesets > 1 & ignore_diff == F){
      stop(" Column names are not the same across all files. It would be a good idea to visually inspect them.")
    }

    colnames(temp) <- col_names # ok, because I already checked to make sure the column name differences were innocuous


    # save individual cleaned file as a new rds file
    saveRDS(temp, gsub(".rds","_cleaned.rds",file_name))
  }
}




