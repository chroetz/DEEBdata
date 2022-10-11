
checkConditionStopped <- function(traj, fun, parms, thershold) {
  idxLast <- nrow(traj)
  duLast <- unlist(fun(traj$time[idxLast], traj$state[idxLast,], parms))
  duLastNorm <- sqrt(sum(duLast^2))
  return(duLastNorm >= thershold)
}

countTurns <- function(x, time, threshold) {
  v <- diff(x) / diff(time)
  y <- v[abs(v) >= threshold]
  sum(abs(diff(sign(y))) > 1)
}

checkConditionTurning <- function(traj, threshold, minTurnCount) {
  turnCount <- sum(apply(traj$state, 2, countTurns, time=traj$time, threshold=threshold))
  return(turnCount >= minTurnCount)
}

checkConditionBounded <- function(traj, maxNorm) {
  uNormsSqr <- rowSums(traj$state^2)
  return(all(uNormsSqr <= maxNorm^2))
}

checkCondition <- function(opts, traj, fun, parms) {
  name <- getThisClass(opts)
  fulfilled <- switch(
    name,
    stopped = checkConditionStopped(traj, fun, parms, opts$threshold),
    bounded = checkConditionBounded(traj, opts$maxNorm),
    turning = checkConditionTurning(traj, opts$threshold, opts$minTurnCount),
    true = TRUE,
    stop("Unrecognized name ", name))
  return(fulfilled)
}

checkConditions <- function(opts, traj, fun, parms) {
  if (length(opts$list) == 0) return(TRUE)
  conditionsFulfilled <- sapply(opts$list, checkCondition, traj=traj, fun=fun, parms=parms)
  return(isTRUE(all(conditionsFulfilled)))
}
