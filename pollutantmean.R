pollutantmean <- function(directory, pollutant, id) {  
  files_full <- list.files(directory, full.names=TRUE)
  data <- read.csv(files_full[id])
  vals <- data[[pollutant]]
  na_vals <- is.na(vals)
  numbers <- vals[!na_vals]
  mean(numbers)
}


