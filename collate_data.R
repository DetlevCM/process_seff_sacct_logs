
## a secondary script to process the csv files created by read_data.R
## this combines all individual csv datasets into one single csv dataset


## obtain an overview of all days for which there is data:
# https://stackoverflow.com/questions/14958516/read-all-files-in-directory-and-apply-multiple-functions-to-each-data-frame
GetAllDays <- function() {
  list.files(path = ".", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
}

## check if a csv file with all data exists and remove it before processing
if (file.exists("all_data.csv")) {
  unlink("all_data.csv")
}

files <- GetAllDays()


## merge the csv files into one dataframe
# https://stackoverflow.com/questions/30242065/trying-to-merge-multiple-csv-files-in-r
csv_multi_merge <- function(files) {
  lst <- lapply(files, function(x) {
    read.csv(x, header = TRUE, stringsAsFactors = FALSE)
  })
  do.call("rbind", lst)
}

data <- csv_multi_merge(files)

## write the resulting data, so it is available for inspection of storing
write.csv(data, file = "all_data.csv",  row.names = FALSE)
