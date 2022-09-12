getParmsFunction <- function(opts) {
  parmsFunction <- switch(
    opts$name,
    localConst = getParmsFunctionLocalConst(opts),
    polynomial = getParmsFunctionPolynomial(opts),
    lotkaVolterra = getParmsFunctionLotkaVolterra(opts),
    lorenz63 = getParmsFunctionLorenz63(opts),
    stop("Unrecognized name ", opts$className))
  return(parmsFunction)
}


buildParmsSampler <- function(opts) {
  parmsSampler <- switch(
    opts$name,
    localConst = buildParmsSamplerLocalConst(opts),
    polynomial = buildParmsSamplerPolynomial(opts),
    lotkaVolterra = buildParmsSamplerLotkaVolterra(opts),
    lorenz63 = buildParmsSamplerLorenz63(opts),
    stop("Unrecognized name ", opts$className))
  return(parmsSampler)
}
