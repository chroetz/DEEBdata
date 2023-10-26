sampleGpParmsSuiteableForLorenz <- function(opts) {

  for (k in seq_len(opts$maxTries)) {
    parms <- sampleGaussianProcess(n = opts$nSupportPoints, d = 3) # default parameters: [0,1]^3 -> [-1,1]^3
    suitable <- checkParmsSuitable(parms, opts)
    if (suitable) return(parms)
  }

  stop("Could not find suitable GP.")
}


checkParmsSuitable <- function(parms, opts) {
  time <- seq(0, opts$maxTime, length.out = opts$odeSteps)
  res <- deSolve::ode(
    opts$u0, time,
    \(t, u, parms) list(lorenz63(u, getLorenzNonparamCoef(u, parms, opts))),
    parms, "rk4")
  if (any(is.na(res))) return(FALSE)
  resTailStates <- res[(opts$odeSteps*opts$warmUpRatio):opts$odeSteps, -1]
  finalVariation <- mean(abs(apply(resTailStates, 2, diff)))
  if (finalVariation < opts$minVariation) return(FALSE)
  return(TRUE)
}

getLorenzNonparamCoef <- function(u, parms, opts) {
  gpIn <- (u + opts$gpInOffset)*opts$gpInScale
  gpOut <- DEEButil::evalGaussianProcess(
    gpIn, parms$locations, parms$weights, bandwidth = opts$gpBandwidth)
  coef <- gpOut*opts$gpOutScale + opts$gpOutOffset
  return(coef)
}


getParmsFunctionLorenzNonparam <- function(opts) {
  opts <- asOpts(opts, c("lorenzNonparam", "Function", "Sampler"))
  parmsFunction <- function(t, u, parms) {
    coef <- getLorenzNonparamCoef(u, parms, opts)
    du <- lorenz63(u, coef)
    return(du)
  }
  return(parmsFunction)
}


buildParmsSamplerLorenzNonparam <- function(opts) {
  opts <- asOpts(opts, c("lorenzNonparam", "Function", "Sampler"))
  sample <- \() sampleGpParmsSuiteableForLorenz(opts)
  return(sample)
}
