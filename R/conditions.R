
checkConditionStopped <- function(traj, fun, parms, thershold) {
  idxLast <- nrow(traj)
  duLast <- unlist(fun(traj$time[idxLast], traj$state[idxLast,], parms))
  duLastNorm <- sqrt(sum(duLast^2))
  return(duLastNorm >= thershold)
}

checkConditionFinite <- function(traj) {
  return(all(is.finite(traj$state)))
}

checkConditionBounded <- function(traj, maxNorm) {
  uNormsSqr <- rowSums(traj$state^2)
  return(all(uNormsSqr <= maxNorm^2))
}

checkCondition <- function(opts, traj, fun, parms) {
  fulfilled <- switch(
    opts$name,
    stopped = checkConditionStopped(traj, fun, parms, opts$threshold),
    finite = checkConditionFinite(traj),
    bounded = checkConditionBounded(traj, opts$maxNorm),
    true = TRUE,
    stop("Unrecognized name ", opts$name))
  return(fulfilled)
}

checkConditions <- function(opts, traj, fun, parms) {
  if (length(opts$list) == 0) return(TRUE)
  conditionsFulfilled <- sapply(opts$list, checkCondition, traj=traj, fun=fun, parms=parms)
  return(isTRUE(all(conditionsFulfilled)))
}
