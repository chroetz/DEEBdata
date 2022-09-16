run <- function(x) {
  opts <- asOpts(x, "Run")
  sampleTrajectories(opts$truthOpts)
  generateObservations(opts$noiseOpts)
  plotTogether(opts$plotOpts)
}
