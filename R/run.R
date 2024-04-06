#' @export
run <- function(
    x,
    overwriteList = NULL,
    truth = TRUE, obs = TRUE, task = TRUE, plot = TRUE,
    randomizeSeed = FALSE) {

  opts <- asOpts(x, "Run")

  if (randomizeSeed) {
    opts$truth$seed <- sample.int(.Machine$integer.max, 1)
    for (k in seq_along(opts$observation$list))
      opts$observation$list[[k]]$seed <- sample.int(.Machine$integer.max, 1)
  }

  opts <- overwriteOpts(opts, overwriteList)

  wd <- getwd()
  on.exit(setwd(wd))

  modelPath <- file.path(opts$path, opts$name)
  if (!dir.exists(modelPath)) dir.create(modelPath, recursive=TRUE)
  modelPath <- normalizePath(modelPath, winslash="/", mustWork=TRUE)

  message("Entering directory ", modelPath)
  setwd(modelPath)

  writeOpts(opts, "Opts_Run")

  if (!dir.exists("estimation")) dir.create("estimation")
  if (truth) sampleTrajectoriesAndWriteForTasks(
    opts$truth,
    opts$taskList,
    opts$observation,
    writeOpts = FALSE)
  if (obs) generateObservations(opts$observation, writeOpts = FALSE)
  if (task) writeTasks(opts$taskList, writeOpts = FALSE)
  if (plot) plotTogether(opts$plot, writeOpts = FALSE)
}


#' @export
getRunOptsFiles <- function(
    fullPath = FALSE,
    fileEnding = fullPath,
    optsPath = NULL,
    fromPackage = TRUE
) {
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
    overwrite = NULL,
    reps = NULL,
    truth = TRUE, obs = TRUE, task = TRUE, plot = TRUE,
    randomizeSeed = FALSE
) {
  files <- getRunOptsFiles(fullPath = TRUE, optsPath = optsPath, fromPackage = fromPackage)
  if (!is.null(pattern)) files <- files[grepl(pattern, basename(files))]
  overwrite <- c(overwrite, list(path = dbPath))
  if (!is.null(reps)) {
    overwrite$truth <- c(overwrite$truth, list(reps = reps))
  }
  for (fl in files) {
    run(
      fl,
      overwriteList = overwrite,
      truth = truth, obs = obs, task = task, plot = plot,
      randomizeSeed = randomizeSeed)
  }
  dbPath <- normalizePath(dbPath)
  return(dbPath)
}
