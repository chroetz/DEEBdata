getParmsFunctionGaussianProcess <- function(opts) {
  parmsFunction <- function(t, u, parms) {
    du <- DEEButil::evalGaussianProcess(u, parms$locations, parms$weights, bandwidth = opts$bandwidth)
    return(du)
  }
  return(parmsFunction)
}


buildParmsSamplerGaussianProcess <- function(opts) {
  locationSampler <- buildArraySampler(
    opts$locationSampler,
    arrayDim = c(opts$nSupp, opts$d))
  valueSampler <- buildArraySampler(
    opts$valueSampler,
    arrayDim = c(opts$nSupp, opts$d))
  parmsSampler <- function() {
    locations <- locationSampler()
    values <- valueSampler()
    weights <- DEEButil::calculateGaussianProcessWeights(
      locations,
      values,
      bandwidth = opts$bandwidth,
      regulation = opts$regulation)
    return(lst(locations, weights))
  }
  return(parmsSampler)
}
