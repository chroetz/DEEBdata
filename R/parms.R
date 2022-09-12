getParmsFunction <- function(opts) {
  parmsFunction <- switch(
    opts$className,
    localConst = getParmsFunctionLocalConst(opts),
    polynomial = getParmsFunctionPolynomial(opts),
    stop("Unrecognized name ", opts$className))
  return(parmsFunction)
}


buildParmsSampler <- function(opts) {
  parmsSampler <- switch(
    opts$className,
    localConst = buildParmsSamplerLocalConst(opts),
    polynomial = buildParmsSamplerPolynomial(opts),
    stop("Unrecognized name ", opts$className))
  return(parmsSampler)
}
