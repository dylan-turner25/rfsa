
# get all the cleaned files to pass to the split payment file function
cleaned_files <- list.files(path = "./data-raw/fsaFarmPayments/input_data",
                            pattern = "*cleaned.rds", full.names = TRUE)

# Note, don't parallelize this function because it will try to
# periodically access the same state file at the same time via two seperate cores

# for(file in cleaned_files){
#   print(file)
#   split_file(readRDS(file), year_type = "payment")
#   split_file(readRDS(file), year_type = "program") # tend to be missing values
#   split_file(readRDS(file), year_type = "fiscal")
# }

library(cli)
n <- length(cleaned_files)

# start a progress bar
pb <- cli_progress_bar(
  "Splitting files",
  total   = n
)

for (file in cleaned_files) {
  # advance the bar by 1
  #cli_progress_update()
  cli_progress_update()

  split_file(readRDS(file), year_type = "payment")
  split_file(readRDS(file), year_type = "program")
  split_file(readRDS(file), year_type = "fiscal")
}

# finish it off
cli_progress_done()

