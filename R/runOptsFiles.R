#' @export
getRunOptsFiles <- function(fullPath = FALSE, fileEnding = fullPath, subfolder = NULL) {
  if (length(subfolder) == 0) subfolder <- ""
  runOptsPath <- system.file(
    file.path("runOpts", subfolder),
    package = "DEEBdata")
  files <- dir(runOptsPath, full.names=fullPath)
  if (!fileEnding)
    files <- substr(files, 1, nchar(files) - 5)
  return(files)
}

#' @export
runAll <- function(
    dbPath = paste0("~/DEEBDB", reps),
    pattern = NULL,
    subfolder = NULL,
    createExample = FALSE,
    reps = NULL,
    truth = TRUE, obs = TRUE, task = TRUE, plot = TRUE
) {
  files <- getRunOptsFiles(fullPath = TRUE, subfolder = subfolder)
  if (!is.null(pattern)) files <- files[grepl(pattern, basename(files))]
  overwrite <- list(path = dbPath)
  if (!is.null(reps)) {
    overwrite$truth = list(reps = reps)
  }
  for (fl in files) {
    run(fl, createExample = createExample, overwriteList = overwrite, truth = truth, obs = obs, task = task, plot = plot)
  }
  dbPath <- normalizePath(dbPath)
  return(dbPath)
}
