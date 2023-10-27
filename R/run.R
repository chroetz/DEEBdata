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
    opts$truth,
    opts$taskList,
    opts$observation,
    writeOpts = FALSE)
  if (obs) generateObservations(opts$observation, writeOpts = FALSE)
  if (task) writeTasks(opts$taskList, writeOpts = FALSE)
  if (plot) plotTogether(opts$plot, writeOpts = FALSE)

  if (createExample) {
    message("Creating Example")
    dir.create("example")
    setwd("example")
    opts$truth$seed <- sample.int(.Machine$integer.max, 1)
    opts$observation$seed <- sample.int(.Machine$integer.max, 1)
    writeOpts(opts, "Opts_Run")
    if (truth) sampleTrajectoriesAndWriteForTasks(
      opts$truth,
      opts$taskList,
      opts$observation,
      writeOpts = FALSE)
    if (obs) generateObservations(opts$observation, writeOpts = FALSE)
    if (task) writeTasks(opts$taskList, writeOpts = FALSE)
    if (plot) plotTogether(opts$plot, writeOpts = FALSE)
  }
}
