sampleGaussianProcess <- function(
    n, d,
    locationRange = c(0, 1),
    valueRange = c(-1, 1),
    regulation = 1e-7,
    bandwidth = 1
) {
  locations <- matrix(
    stats::runif(n*d, min = locationRange[1], max = locationRange[2]),
    ncol = d)
  values <- matrix(
    stats::runif(n*d, min = valueRange[1], max = valueRange[2]),
    ncol = d)
  weights <- DEEButil::calculateGaussianProcessWeights(
    locations,
    values,
    bandwidth = bandwidth,
    regulation = regulation)
  return(lst(locations, weights))
}
