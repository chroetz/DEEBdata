#' @export
run <- function(x, createExample = FALSE, overwriteList = NULL) {

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
  sampleTrajectoriesAndWriteForTasks(
    opts$truthOpts,
    opts$taskList,
    opts$observationOpts,
    writeOpts = FALSE)
  generateObservations(opts$observationOpts, writeOpts = FALSE)
  writeTasks(opts$taskList, writeOpts = FALSE)
  plotTogether(opts$plotOpts, writeOpts = FALSE)

  if (createExample) {
    dir.create("example")
    setwd("example")
    opts$truthOpts$seed <- sample.int(.Machine$integer.max, 1)
    opts$observationOpts$seed <- sample.int(.Machine$integer.max, 1)
    writeOpts(opts, "Opts_Run")
    sampleTrajectoriesAndWriteForTasks(
      opts$truthOpts,
      opts$taskList,
      opts$observationOpts,
      writeOpts = FALSE)
    generateObservations(opts$observationOpts, writeOpts = FALSE)
    writeTasks(opts$taskList, writeOpts = FALSE)
    plotTogether(opts$plotOpts, writeOpts = FALSE)
  }
}
