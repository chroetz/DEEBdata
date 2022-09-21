getParmsFunctionLotkaVolterra <- function(opts) {
  parmsFunction <- function(t, u, parms) {
    du <- c(
      parms$coef[1] * u[1] - parms$coef[2] * u[1] * u[2],
      parms$coef[3] * u[1] * u[2] - parms$coef[4] * u[2]
    )
    return(du)
  }
  return(parmsFunction)
}


buildParmsSamplerLotkaVolterra <- function(opts) {
  coefSampler <- buildArraySampler(
    opts$coefSampler,
    arrayDim = 4)
  parmsSampler <- function() list(coef = as.vector(coefSampler()))
  return(parmsSampler)
}
