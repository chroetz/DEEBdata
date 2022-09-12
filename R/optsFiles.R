writeOpts <- function(opts, file) {
  opts$timeStamp <- date()
  opts$version <- getPackageVersion()
  if (is.character(file) && !endsWith(file, ".json")) {
    file <- paste0(file, ".json")
  }
  jsonlite::write_json(opts, file, pretty = TRUE, digits = 8, auto_unbox = TRUE)
}


readOpts <- function(opts) {
  if (is.list(opts)) return(opts)
  jsonlite::read_json(opts, simplifyVector = TRUE)
}

getPackageVersion <- function() {
  format(utils::packageVersion(utils::packageName()))
}
