getParmsFunction <- function(opts) {
  f <- switch(
    opts$name,
    const = getParmsFunctionConst(opts),
    localConst = getParmsFunctionLocalConst(opts),
    polynomial = getParmsFunctionPolynomial(opts),
    lotkaVolterra = getParmsFunctionLotkaVolterra(opts),
    lorenz63 = getParmsFunctionLorenz63(opts),
    stop("Unrecognized name ", opts$name))
  parmsFunction <- composeWithPostprocessors(f, opts$postprocessors)
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
    stop("Unrecognized name ", opts$name))
  parmsSamplerWithClass <- function() {
    res <- parmsSampler()
    oldClass(res) <- c(paste0(opts$name, "ParmsOpts"), "ParmsOpts", "Opts")
    return(res)
  }
  return(parmsSampler)
}


