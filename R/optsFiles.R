writeOpts <- function(opts, file) {
  opts$timeStamp <- date()
  opts$version <- getPackageVersion()
  if (is.character(file) && !endsWith(file, ".json")) {
    file <- paste0(file, ".json")
  }
  opts <- putClassAttributAsListEntry(opts)
  jsonlite::write_json(unclass(opts), file, pretty = TRUE, digits = 8, auto_unbox = TRUE)
}


readOpts <- function(file, optsClass = NULL, .fill = TRUE) {
  fileContent <- jsonlite::read_json(file, simplifyVector = TRUE)
  opts <- putListEntryClassAsAttribute(fileContent)
  # nonStandardEntries <- opts[startsWith(names(opts), "_")]
  opts <- opts[!startsWith(names(opts), "_")] # remove non-standard entries
  if (is.null(optsClass)) {
    optsClass <- oldClass(opts)[1]
  }
  opts <- asOpts(opts, optsClass, .fill = .fill)
  validateOpts(opts)
}

putListEntryClassAsAttribute <- function(lst) {
  if ("_class" %in% names(lst)) {
    oldClass(lst) <- lst[["_class"]]
    lst[["_class"]] <- NULL
  }
  for (i in seq_along(lst)) {
    if (is.list(lst[[i]]))
      lst[[i]] <- putListEntryClassAsAttribute(lst[[i]])
  }
  return(lst)
}

putClassAttributAsListEntry <- function(obj) {
  obj[["_class"]] <- oldClass(obj)
  obj <- unclass(obj)
  for (i in seq_along(obj)) {
    if (is.list(obj[[i]]))
      obj[[i]] <- putClassAttributAsListEntry(obj[[i]])
  }
  return(obj)
}

getPackageVersion <- function() {
  format(utils::packageVersion(utils::packageName()))
}
