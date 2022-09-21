sampleLorenz63 <- function(coeffSampler) {
  coef <- coeffSampler(3)
  fOpts <- list()
  fOpts$A <- rbind(
    c(-coef[1], coef[1], 0, 0, 0, 0),
    c(coef[2], -1, 0, 0, -1, 0),
    c(0, 0, -coef[3], 1, 0, 0))
  fun <- function(t, u, parms) {
    list(fOpts$A %*% c(u, u[1]*u[2], u[1]*u[3], u[2]*u[3]))
  }
  return(fun)
}


getParmsFunctionLorenz63 <- function(opts) {
  parmsFunction <- function(t, u, parms) {
    du <- c(
      parms$coef[1] * (u[2]- u[1]),
      parms$coef[2] * u[1] - u[2] - u[1] * u[3],
      u[1] * u[2] - parms$coef[3] * u[3]
    )
    return(du)
  }
  return(parmsFunction)
}


buildParmsSamplerLorenz63 <- function(opts) {
  coefSampler <- buildArraySampler(
    opts$coefSampler,
    arrayDim = 3)
  parmsSampler <- function() list(coef = as.vector(coefSampler()))
  return(parmsSampler)
}
