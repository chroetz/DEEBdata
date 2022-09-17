getParmsFunctionPolynomial <- function(opts) {

  p <- numberOfTermsInPoly(opts$polyDeg, opts$d)
  degVecs <- as.matrix(expand.grid(rep(list(0:opts$polyDeg), opts$d)))
  degVecs <- degVecs[rowSums(degVecs) <= opts$polyDeg, , drop=FALSE]
  stopifnot(nrow(degVecs) == p)

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
    arrayDim = c(opts$d, numberOfTermsInPoly(opts$polyDeg, opts$d)))
  parmsSampler <- function() list(coef = coefSampler())
  return(parmsSampler)
}


numberOfTermsInPoly <- \(polyDeg, d) {
  sum(sapply(0:polyDeg, \(deg) choose(d+deg-1, deg)))
}
