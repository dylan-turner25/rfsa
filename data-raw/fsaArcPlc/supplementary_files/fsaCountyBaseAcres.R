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

  # add a program year column
  temp$program_year <- y

  assign(paste0("data_", y), temp)

}

# clean 2021, 2022, and 2023
for(y in 2021:2023){
  # assign df to temporary object
  temp <- eval(parse(text = paste0("data_", y)))

  # clean names using janitor
  temp <- janitor::clean_names(temp)

  names(temp) <- c("program_year","state","county","crop","base_acres")

  assign(paste0("data_", y), temp)

}

# combine all data frames
county_base_acres <- dplyr::bind_rows(data_2014, data_2015, data_2021, data_2022, data_2023)

# extract crops codes
county_base_acres$extracted_crop_type <- unlist(lapply(county_base_acres$crop, extract_crop_type))
county_base_acres$extracted_crop_type2 <- unlist(lapply(paste0( county_base_acres$crop," ",county_base_acres$crop_type), extract_crop_type))

county_base_acres <- county_base_acres %>%
  mutate(crop_type = case_when(
    !is.na(extracted_crop_type) ~ extracted_crop_type,
    !is.na(extracted_crop_type2) ~ extracted_crop_type2,
    TRUE ~ NA_character_
  ))

# add rma crop codes where applicable
county_base_acres$rma_type_code <- unlist(lapply(paste0(county_base_acres$crop," ",county_base_acres$crop_type ), extract_crop_type, rma_code = TRUE))

# clean crop names
county_base_acres$crop <- unlist(lapply(county_base_acres$crop, clean_crop_names2))

# add rma crop codes
county_base_acres$rma_crop_code <- unlist(lapply(county_base_acres$crop, assign_rma_cc))

# drop any unneeded columns
county_base_acres <- county_base_acres %>%
  select(-extracted_crop_type, -extracted_crop_type2)

# get fips codes for each state-county pair
library(dplyr)
library(stringr)
library(purrr)
library(fipio)

unique_fips <- county_base_acres %>%
  filter(!is.na(state)) %>%
  distinct(state, county) %>%
  mutate(
    fips = pmap_chr(
      list(state, county),
      function(st, ct) {
        # derive candidate names
        parts <- str_split(ct, "-", simplify = TRUE)
        variants <- unique(c(
          ct,
          parts[1],
          str_remove_all(str_to_lower(parts[1]), "west|south|north|east"),
          parts[2]
        ))
        variants <- variants[!is.na(variants) & variants != ""]
        # attempt lookup and take first non-NA
        out <- variants %>%
          map_chr(~ as_fips(st, .x)) %>%
          na.omit()
        if (length(out)) return(out[1]) else return(NA_character_)
      }
    ),
    # uppercase for consistency
    state  = str_to_upper(state),
    county = str_to_upper(county),
    # manual overrides for known mismatches
    fips = case_when(
      state == "ILLINOIS" & county == "DEWITT" ~ "19045",
      state == "MAINE"     & county %in% c("FORT KENT", "HOULTON") ~ "23003",
      TRUE ~ fips
    )
  ) %>%
  select(state, county, fips) %>%
  distinct()

# merge the fips codes back into the main data frame
county_base_acres <- dplyr::left_join(county_base_acres, unique_fips)

# convert to a tibble before exporting
fsaCountyBaseAcres <- dplyr::as_tibble(county_base_acres)

# use the county level file in the package data folder
usethis::use_data(fsaCountyBaseAcres , overwrite = TRUE)



