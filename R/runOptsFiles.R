#' @export
getRunOptsFiles <- function(fullPath = FALSE, fileEnding = fullPath, optsPath = NULL, fromPackage = TRUE) {
  if (length(optsPath) == 0) optsPath <- ""
  if (fromPackage) {
    runOptsPath <- system.file(
      file.path("runOpts", optsPath),
      package = "DEEBdata")
  } else {
    runOptsPath <- normalizePath(optsPath, mustWork=TRUE)
  }
  files <- dir(runOptsPath, full.names=fullPath)
  if (!fileEnding)
    files <- substr(files, 1, nchar(files) - 5)
  return(files)
}

#' @export
runAll <- function(
    dbPath = paste0("~/DEEBDB", reps),
    pattern = NULL,
    optsPath = NULL,
    fromPackage = TRUE,
    createExample = FALSE,
    overwrite = NULL,
    reps = NULL,
    truth = TRUE, obs = TRUE, task = TRUE, plot = TRUE
) {
  files <- getRunOptsFiles(fullPath = TRUE, optsPath = optsPath, fromPackage = fromPackage)
  if (!is.null(pattern)) files <- files[grepl(pattern, basename(files))]
  overwrite <- c(overwrite, list(path = dbPath))
  if (!is.null(reps)) {
    overwrite$truth <- c(overwrite$truth, list(reps = reps))
  }
  for (fl in files) {
    run(fl, createExample = createExample, overwriteList = overwrite, truth = truth, obs = obs, task = task, plot = plot)
  }
  dbPath <- normalizePath(dbPath)
  return(dbPath)
}
