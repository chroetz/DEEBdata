run <- function(x) {
  opts <- asOpts(x, "Run")
  checkOptsHasDefault(opts$truthOpts)
  sampleTrajectories(opts$truthOpts)
  checkOptsHasDefault(opts$noiseOpts)
  generateObservations(opts$noiseOpts)
  checkOptsHasDefault(opts$plotOpts)
  plotTogether(opts$plotOpts)
}
