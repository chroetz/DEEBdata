#' @export
run <- function(x) {

  opts <- asOpts(x, "Run")

  fullPath <- file.path(opts$path, opts$name)
  if (!dir.exists(fullPath)) dir.create(fullPath, recursive=TRUE)
  fullPath <- normalizePath(fullPath, winslash="/", mustWork=TRUE)

  message("Entering directory ", fullPath)

  wd <- getwd()
  setwd(fullPath)
  on.exit(setwd(wd))

  writeOpts(opts, file.path(fullPath, "Opts_Run"))

  sampleTrajectories(opts$truthOpts, writeOpts = FALSE)
  generateObservations(opts$observationOpts, writeOpts = FALSE)
  plotTogether(opts$plotOpts, writeOpts = FALSE)
}
