run <- function(x) {
  if (inherits(x, "Opts")) {
    opts <- x
  } else if (is.character(x)) {
    if (file.exists(x)) {
      opts <- readOpts(x, "RunOpts")
    } else {
      stop("Trying to interprete x as path, but file does not exist. x = ", x)
    }
  } else {
    stop("invalid argument x")
  }
  checkOptsHasDefault(opts$truthOpts)
  sampleTrajectories(opts$truthOpts)
  checkOptsHasDefault(opts$noiseOpts)
  generateObservations(opts$noiseOpts)
  checkOptsHasDefault(opts$plotOpts)
  plotTogether(opts$plotOpts)
}
