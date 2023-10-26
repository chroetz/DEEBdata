lorenz63 <- function(u, coef) {
  du <- c(
    coef[1] * (u[2] - u[1]),
    coef[2] * u[1] - u[2] - u[1] * u[3],
    u[1] * u[2] - coef[3] * u[3]
  )
  return(du)
}


getParmsFunctionLorenz63 <- function(opts) {
  parmsFunction <- function(t, u, parms) {
    du <- lorenz63(u, parms$coef)
    return(du)
  }
  return(parmsFunction)
}


buildParmsSamplerLorenz63 <- function(opts) {
  coefSampler <- buildArraySampler(
    opts$coefSampler,
    arrayDim = c(1, 3))
  parmsSampler <- function() list(coef = as.vector(coefSampler()))
  return(parmsSampler)
}
