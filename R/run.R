#' @export
run <- function(x, writeAsDB = FALSE, overwriteList = NULL) {

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

  if (writeAsDB) {

    dir.create("truth")
    dir.create("observation")
    dir.create("example")
    dir.create("estimation")
    dir.create("evaluation")

    set.seed(opts$seed)

    setwd("truth")
    sampleTrajectories(opts$truthOpts, writeOpts = FALSE)

    setwd("../observation")
    opts$observationOpts$truthPath <- file.path("..", "truth")
    generateObservations(opts$observationOpts, writeOpts = FALSE)
    opts$taskList$path <- "."
    writeTasks(opts$taskList, writeOpts = FALSE)

    setwd("..")
    pltOpts <- opts$plotOpts
    pltOpts$truthPath <- "truth"
    pltOpts$obsPath <- "observation"
    pltOpts$path <- "evaluation"
    plotTogether(pltOpts, writeOpts = FALSE)

    setwd("example")
    opts$truthOpts$seed <- sample.int(.Machine$integer.max, 1)
    opts$observationOpts$seed <- sample.int(.Machine$integer.max, 1)
    opts$observationOpts$truthPath <- "."
    sampleTrajectories(opts$truthOpts, writeOpts = TRUE)
    generateObservations(opts$observationOpts, writeOpts = TRUE)
    plotTogether(opts$plotOpts, writeOpts = TRUE)
    writeTasks(opts$taskList, writeOpts = TRUE)

  } else {

    sampleTrajectories(opts$truthOpts, writeOpts = FALSE)
    generateObservations(opts$observationOpts, writeOpts = FALSE)
    plotTogether(opts$plotOpts, writeOpts = FALSE)
    writeTasks(opts$taskList, writeOpts = FALSE)
  }
}
