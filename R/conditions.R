
checkConditionStopped <- function(u, f, parms, thershold) {
  duLast <- unlist(f(u[nrow(u),1], u[nrow(u),-1], parms))
  duLastNorm <- sqrt(sum(duLast^2))
  return(duLastNorm >= thershold)
}

checkConditionFinite <- function(u) {
  return(all(is.finite(u)))
}

checkConditionBounded <- function(u, maxNorm) {
  uNormsSqr <- rowSums(u[,-1]^2)
  return(all(uNormsSqr <= maxNorm^2))
}

checkCondition <- function(opts, u, f, parms) {
  fulfilled <- switch(
    opts$name,
    stopped = checkConditionStopped(u, f, parms, opts$threshold),
    finite = checkConditionFinite(u),
    bounded = checkConditionBounded(u, opts$maxNorm),
    stop("Unrecognized name ", opts$name))
  return(fulfilled)
}

checkConditions <- function(opts, u, f, parms) {
  if (length(opts) == 0) return(TRUE)
  conditionsFulfilled <- sapply(opts, checkCondition, u=u, f=f, parms=parms)
  return(isTRUE(all(conditionsFulfilled)))
}
