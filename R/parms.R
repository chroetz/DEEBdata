getParmsFunction <- function(opts) {
  parmsFunction <- switch(
    opts$name,
    const = getParmsFunctionConst(opts),
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
    const = buildParmsSamplerConst(opts),
    localConst = buildParmsSamplerLocalConst(opts),
    polynomial = buildParmsSamplerPolynomial(opts),
    lotkaVolterra = buildParmsSamplerLotkaVolterra(opts),
    lorenz63 = buildParmsSamplerLorenz63(opts),
    stop("Unrecognized name ", opts$className))
  parmsSamplerWithClass <- function() {
    res <- parmsSampler()
    oldClass(res) <- c(paste0(opts$name, "ParmsOpts"), "ParmsOpts", "Opts")
    return(res)
  }
  return(parmsSampler)
}


