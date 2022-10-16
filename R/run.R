#' @export
run <- function(x, createExample = FALSE, overwriteList = NULL, truth = TRUE, obs = TRUE, task = TRUE, plot = TRUE) {

  opts <- asOpts(x, "Run")

  opts <- overwriteOpts(opts, overwriteList)

  wd <- getwd()
  on.exit(setwd(wd))

  modelPath <- file.path(opts$path, opts$name)
  if (!dir.exists(modelPath)) dir.create(modelPath, recursive=TRUE)
  modelPath <- normalizePath(modelPath, winslash="/", mustWork=TRUE)

  message("Entering directory ", modelPath)
  setwd(modelPath)

  writeOpts(opts, "Opts_Run")

  dir.create("estimation")
  if (truth) sampleTrajectoriesAndWriteForTasks(
    opts$truthOpts,
    opts$taskList,
    opts$observationOpts,
    writeOpts = FALSE)
  if (obs) generateObservations(opts$observationOpts, writeOpts = FALSE)
  if (task) writeTasks(opts$taskList, writeOpts = FALSE)
  if (plot) plotTogether(opts$plotOpts, writeOpts = FALSE)

  if (createExample) {
    dir.create("example")
    setwd("example")
    opts$truthOpts$seed <- sample.int(.Machine$integer.max, 1)
    opts$observationOpts$seed <- sample.int(.Machine$integer.max, 1)
    writeOpts(opts, "Opts_Run")
    if (truth) sampleTrajectoriesAndWriteForTasks(
      opts$truthOpts,
      opts$taskList,
      opts$observationOpts,
      writeOpts = FALSE)
    if (obs) generateObservations(opts$observationOpts, writeOpts = FALSE)
    if (task) writeTasks(opts$taskList, writeOpts = FALSE)
    if (plot) plotTogether(opts$plotOpts, writeOpts = FALSE)
  }
}
