# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaArcCoBenchmarks",
                    full.names = T)

# initialize a data frame to store arc-ic prices
arc_co_benchmarks <- NULL


# load and clean the combined 2014-2018 file ----------------------------------------
arc_co_benchmarks <- readxl::read_excel(files[grepl("2014-2018",files)], skip = 3) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-c(1:6), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# take first 4 characters of the variable column as year
arc_co_benchmarks$program_year <- substr(arc_co_benchmarks$variable, 1, 4)

# remove the first 4 characters from the variable column
arc_co_benchmarks$variable <- trimws(substr(arc_co_benchmarks$variable, 5, nchar(arc_co_benchmarks$variable)))

# pivot wider
arc_co_benchmarks <- arc_co_benchmarks %>%
  pivot_wider(names_from = variable, values_from = value)

# pull out all the columns that have "Bench mark price" in the column title into a single df
bench_mark_price <- arc_co_benchmarks[,grepl("bench mark price", tolower(colnames(arc_co_benchmarks)))]

# remove all columns that have "bench mark price" in the column title
arc_co_benchmarks <- arc_co_benchmarks[,!grepl("bench mark price", tolower(colnames(arc_co_benchmarks)))]

# pull out all the remaining columns that have "bench mark" in the column title into a single df
bench_mark <- arc_co_benchmarks[,grepl("bench mark", tolower(colnames(arc_co_benchmarks)))]

# remove all columns that have "bench mark" in the column title
arc_co_benchmarks <- arc_co_benchmarks[,!grepl("bench mark", tolower(colnames(arc_co_benchmarks)))]

# row sum bench_mark_price and replace 0 with NA
bench_mark_price <- bench_mark_price %>%
  mutate(across(everything(), as.numeric)) %>%
  rowSums(na.rm = T)
bench_mark_price[bench_mark_price == 0] <- NA

# row sum bench_mark
bench_mark <- bench_mark %>%
  mutate(across(everything(), as.numeric)) %>%
  rowSums(na.rm = T)
bench_mark[bench_mark == 0] <- NA


# add the row sums to the arc_co_benchmarks data frame
arc_co_benchmarks <- arc_co_benchmarks %>%
  mutate(oa_bench_mark_price = bench_mark_price,
         oa_bench_mark_yield = bench_mark)

# add column for oa benchmark years
arc_co_benchmarks$oa_bench_mark_years <- paste0(as.numeric(arc_co_benchmarks$year) - 5, "-", as.numeric(arc_co_benchmarks$year) - 1)

# rename ST_Cty to fips
colnames(arc_co_benchmarks)[which(colnames(arc_co_benchmarks) == "ST_Cty")] <- "fips"


# convert whole data frame to character
arc_co_benchmarks <- arc_co_benchmarks %>%
  mutate(across(everything(), as.character))

# clean column names
arc_co_benchmarks <- janitor::clean_names(arc_co_benchmarks)
arc_co_benchmarks <- arc_co_benchmarks %>%
  select(-x8)

# remove any columns that are all NA
arc_co_benchmarks <- arc_co_benchmarks[, colSums(!is.na(arc_co_benchmarks)) > 0]

# rename county_yield_or_70_percent_of_t to county_yield
colnames(arc_co_benchmarks)[which(colnames(arc_co_benchmarks) == "county_yield_or_70_percent_of_t")] <- "county_yield"

# create a new column for county_yield_type
arc_co_benchmarks$county_yield_type <- "county yield or 70% of T-yield"

# loop over remaining years to clean single year files --------------------------
for(y in 2019:current_year){
  # get file path corresponding to year
  file <- files[grepl(y,files)]

  # load the file to a temporary object
  temp <- readxl::read_excel(file)

  # locate row containing "ST_Cty" which contains column names
  names_row <- which(temp[,1] == "ST_Cty")
  for( k in 1:ncol(temp)){
    if(is.na(temp[names_row + 1,k])){
      temp[names_row + 1,k] <- temp[ names_row ,k]
    }
  }

  # define column names
  colnames(temp) <- temp[names_row,]

  # remove everything above the commodity_row
  temp <- temp[-c(1:(names_row + 1)),]

  # remove sub county column if it exists
  if("Sub County" %in% colnames(temp)){
    temp <- temp %>% select(-c("Sub County"))
  }

  # clean the raw data
  temp <- temp %>%
    pivot_longer(-c(1:6), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))

  # take first 4 characters of the variable column as year
  temp$program_year <- substr(temp$variable, 1, 4)

  # remove the first 4 characters from the variable column
  temp$variable <- trimws(substr(temp$variable, 5, nchar(temp$variable)))

  # identify any duplicate rows
  duplicates <- temp |>
    dplyr::summarise(n = dplyr::n(), .by = c(ST_Cty, `State Name`, `County Name`, `Crop Name`,
                                             Unit, `ARC-CO Yield Designation`, program_year, variable)) |>
    dplyr::filter(n > 1L)

  if(nrow(duplicates) > 0){
    # if there are duplicates, remove them
    temp <- temp %>%
      group_by(ST_Cty, `State Name`, `County Name`, `Crop Name`, Unit,
               `ARC-CO Yield Designation`, program_year, variable) %>%
      summarise(value = mean(as.numeric(value), na.rm = T), .groups = "drop")
  }

  # pivot wider
  temp <- temp %>%
    pivot_wider(names_from = variable, values_from = value)

  # pull out all the columns that have "Bench mark price" in the column title into a single df
  bench_mark_price <-   temp[,grepl("bench mark price", tolower(colnames(  temp)))]

  # remove all columns that have "bench mark price" in the column title
  temp <-   temp[,!grepl("bench mark price", tolower(colnames(  temp)))]

  # pull out all the remaining columns that have "bench mark" in the column title into a single df
  bench_mark <-   temp[,grepl("bench mark", tolower(colnames(  temp)))]

  # remove all columns that have "bench mark" in the column title
  temp <-   temp[,!grepl("bench mark", tolower(colnames(  temp)))]

  # row sum bench_mark_price and replace 0 with NA
  bench_mark_price <- bench_mark_price %>%
    mutate(across(everything(), as.numeric)) %>%
    rowSums(na.rm = T)
  bench_mark_price[bench_mark_price == 0] <- NA

  # row sum bench_mark
  bench_mark <- bench_mark %>%
    mutate(across(everything(), as.numeric)) %>%
    rowSums(na.rm = T)
  bench_mark[bench_mark == 0] <- NA


  # add the row sums to the arc_co_benchmarks data frame
  temp <-   temp %>%
    mutate(oa_bench_mark_price = bench_mark_price,
           oa_bench_mark_yield = bench_mark)

  # add column for oa benchmark years
  temp$oa_bench_mark_years <- paste0(as.numeric(  temp$program_year) - 5, "-", as.numeric(  temp$program_year) - 1)

  # rename ST_Cty to fips
  colnames(  temp)[which(colnames(  temp) == "ST_Cty")] <- "fips"


  # convert whole data frame to character
  temp <-   temp %>%
    mutate(across(everything(), as.character))

  # clean column names
  temp <- janitor::clean_names(  temp)

  # rename  arc_co_yield_designation to yield_type
  colnames(temp)[which(colnames(temp) == "arc_co_yield_designation")] <- "yield_type"

  # rename trend_adjusted_county_yield_or_80_percent_of_t to county_yield
  colnames(temp)[which(colnames(temp) == "trend_adjusted_county_yield_or_80_percent_of_t")] <- "county_yield"

  # create a new column for county_yield_type
  temp$county_yield_type <- "trend adjusted county yield or 80% of T-yield"


  # check if the column names in temp are the same as the column names in arc_co_benchmarks
  if(!all(colnames(arc_co_benchmarks) %in% colnames(temp))){
    # if not, add the missing columns to temp
    for(c in colnames(arc_co_benchmarks)){
      if(!c %in% colnames(temp)){
        temp[,c] <- NA
      }
    }
  }

  # row bind the temp data frame to the arc_co_benchmarks data frame
  arc_co_benchmarks <- dplyr::bind_rows(arc_co_benchmarks, temp)

}

# type convert all columns
arc_co_benchmarks <- readr::type_convert(arc_co_benchmarks)
arc_co_benchmarks$program_year <- as.numeric(arc_co_benchmarks$program_year)

# rename crop_name to crop
colnames(arc_co_benchmarks)[which(colnames(arc_co_benchmarks) == "crop_name")] <- "crop"

# extract crop type
arc_co_benchmarks$crop_type <- unlist(lapply(arc_co_benchmarks$crop, extract_crop_type))

# add rma crop codes where applicable
arc_co_benchmarks$rma_type_code <- unlist(lapply(arc_co_benchmarks$crop, extract_crop_type, rma_code = TRUE))

# clean crop names
arc_co_benchmarks$crop <- unlist(lapply(arc_co_benchmarks$crop, clean_crop_names))

# add rma crop codes
arc_co_benchmarks$rma_crop_code <- unlist(lapply(arc_co_benchmarks$crop, assign_rma_cc))

# drop if actual yield is na
arc_co_benchmarks <- distinct(arc_co_benchmarks %>%
  filter(!is.na(actual_yield)) %>%
  select(-county_yield, -county_yield))


# convert  data to a tibble before exporting
fsaArcCoBenchmarks <- dplyr::as_tibble(arc_co_benchmarks)


# use the county level file in the package data folder
usethis::use_data(fsaArcCoBenchmarks, overwrite = TRUE)



