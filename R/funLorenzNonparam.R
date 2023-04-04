sampleGpLorenz <- function(maxTires = 100) {

  # TODO: disenchant magic constants
  odeSteps <- 1e3
  time <- seq(0, 50, length.out = odeSteps)

  # maybe need to fix this in data generation??
  u0 <- c(-0.32, -0.6,  8) # Starting point on the original Lorenz63 attractor.

  for (k in seq_len(maxTires)) {
    parms <- sampleGaussianProcess(n = 5, d = 3)
    res <- deSolve::ode(
      u0, time,
      \(t, u, parms) list(lorenz63(u, getLorenzNonparamCoef(u, parms))),
      parms, "rk4")
    if (any(is.na(res))) next
    finalVariation <- mean(abs(diff(res[(odeSteps*0.9):odeSteps, 2])))
    if (finalVariation > 1) {
      return(parms)
    }
  }

  stop("Could not find suitable GP.")
}


getLorenzNonparamCoef <- function(u, parms) {
  # TODO: disenchant magic constants
  x <- c((u[1]+50), (u[2]+50), u[3]+25) / 100
  y <- DEEButil::evalGaussianProcess(x, parms$locations, parms$weights, bandwidth = 1)
  c(10+5*y[1], 50+30*y[2], 4+2*y[3])
}


getParmsFunctionLorenzNonparam <- function(opts) {
  parmsFunction <- function(t, u, parms) {
    coef <- getLorenzNonparamCoef(u, parms)
    du <- lorenz63(u, coef)
    return(du)
  }
  return(parmsFunction)
}


buildParmsSamplerLorenzNonparam <- function(opts) {
  return(sampleGpLorenz)
}
