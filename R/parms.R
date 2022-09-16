getParmsFunction <- function(opts) {
  name <- class(opts)[1]
  f <- switch(
    name,
    const = getParmsFunctionConst(opts),
    localConst = getParmsFunctionLocalConst(opts),
    polynomial = getParmsFunctionPolynomial(opts),
    lotkaVolterra = getParmsFunctionLotkaVolterra(opts),
    lorenz63 = getParmsFunctionLorenz63(opts),
    stop("Unrecognized name ", name))
  parmsFunction <- composeWithPostprocessors(f, opts$postprocessors)
  return(parmsFunction)
}


buildParmsSampler <- function(opts) {
  name <- class(opts)[1]
  parmsSampler <- switch(
    name,
    const = buildParmsSamplerConst(opts),
    localConst = buildParmsSamplerLocalConst(opts),
    polynomial = buildParmsSamplerPolynomial(opts),
    lotkaVolterra = buildParmsSamplerLotkaVolterra(opts),
    lorenz63 = buildParmsSamplerLorenz63(opts),
    stop("Unrecognized name ", name))
  parmsSamplerWithClass <- function() {
    res <- parmsSampler()
    opts <- asOpts(res, c(name, "Parms"), .fill = FALSE)
    return(opts)
  }
  return(parmsSamplerWithClass)
}


