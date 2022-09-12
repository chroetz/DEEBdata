getKernel <- function(opts) {
  if (identical(opts, "dnorm")) {
    return(stats::dnorm)
  }
  stop("invalid opts")
}


localConstRegression <- function(x, parms, kernel, bw) {
  n <- nrow(parms$locations)
  dst <-
    (matrix(rep(x, each = n), nrow = n) - parms$locations)^2 |>
    rowSums() |>
    sqrt()
  weight <- kernel(dst / bw)
  weight <- weight / sum(weight)
  y <- weight %*% parms$values
  return(y)
}


getParmsFunctionLocalConst <- function(opts) {
  kernel <- getKernel(opts$kernel)
  parmsFunction <- function(t, u, parms) {
    du <- localConstRegression(u, parms, kernel = kernel, bw = opts$bw)
    uNorm <- sqrt(sum(u^2))
    if (uNorm > 2) {
      return(list(-u))
    } else if (uNorm > 1) {
      return(list(-u * (uNorm-1) + du * (2 - uNorm)))
    }
    list(du)
  }
  return(parmsFunction)
}


buildParmsSamplerLocalConst <- function(opts) {
  locationSampler <- buildMatrixSampler(opts$locationSampler)
  valueSampler <- buildMatrixSampler(opts$valueSampler)
  parmsSampler <- function() list(
    locations = locationSampler(opts$nSupp, opts$d),
    values = valueSampler(opts$nSupp, opts$d))
  return(parmsSampler)
}
