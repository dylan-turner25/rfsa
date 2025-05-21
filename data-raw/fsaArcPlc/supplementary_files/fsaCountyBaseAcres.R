# list all files in the raw data files folder
files <- list.files("./data-raw/fsaArcPlc/input_data/fsaCountyBaseAcres",
                    full.names = T)

# initialize a data frame to store arc-ic prices
county_base_acres <- NULL

# load data files
data_2014 <- readxl::read_excel(files[grepl("2014", files)], sheet = 2)
data_2015 <- readxl::read_excel(files[grepl("2014", files)], sheet = 3)
data_2021 <- readxl::read_excel(files[grepl("2021", files)], sheet = 2)
data_2022 <- readxl::read_excel(files[grepl("2022", files)], sheet = 2)
data_2023 <- readxl::read_excel(files[grepl("2023", files)], sheet = 2)

# clean 2014 and 2015
for(y in 2014:2015){

  # assign df to temporary object
  temp <- eval(parse(text = paste0("data_", y)))

  # clean names using janitor
  temp <- janitor::clean_names(temp)



}






