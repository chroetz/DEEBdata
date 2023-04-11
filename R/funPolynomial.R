getParmsFunctionPolynomial <- function(opts) {

  degVecs <- DEEButil::getMonomialExponents(d, opts$polyDeg)

  parmsFunction <- function(t, u, parms) {
    if (any(abs(u) > opts$boundary)) return(list(rep(0, length(u))))
    features <- apply(rep(u, each = nrow(degVecs))^degVecs, 1, prod)
    du <- parms$coef %*% features
    return(du)
  }

  return(parmsFunction)
}


buildParmsSamplerPolynomial <- function(opts) {
  coefSampler <- buildArraySampler(
    opts$coefSampler,
    arrayDim = c(opts$d, DEEButil::numberOfTermsInPoly(opts$polyDeg, opts$d)))
  parmsSampler <- function() list(coef = coefSampler())
  return(parmsSampler)
}
