checkConditions <- function(opts, traj, fun, parms) {
  for (condOpts in opts$list) {
    ok <- checkCondition(condOpts, traj, fun, parms)
    if (!isTRUE(ok)) return(FALSE)
  }
  return(TRUE)
}


checkCondition <- function(opts, traj, fun, parms) {
  name <- getThisClass(opts)
  fulfilled <- switch(
    name,
    stopped = checkConditionStopped(traj, fun, parms, opts$threshold),
    bounded = checkConditionBounded(traj, opts$maxNorm),
    turning = checkConditionTurning(traj, opts$threshold, opts$minTurnCount),
    periodic = checkConditionPeriodic(traj, opts$threshold, opts$kthLargest),
    true = TRUE,
    stop("Unrecognized name ", name))
  return(fulfilled)
}


checkConditionStopped <- function(traj, fun, parms, thershold) {
  idxLast <- nrow(traj)
  duLast <- unlist(fun(traj$time[idxLast], traj$state[idxLast,], parms))
  duLastNorm <- sqrt(sum(duLast^2))
  return(duLastNorm >= thershold)
}

checkConditionBounded <- function(traj, maxNorm) {
  uNormsSqr <- rowSums(traj$state^2)
  return(all(uNormsSqr <= maxNorm^2))
}

checkConditionTurning <- function(traj, threshold, minTurnCount) {
  turnCount <- sum(apply(traj$state, 2, countTurns, time=traj$time, threshold=threshold))
  return(turnCount >= minTurnCount)
}

countTurns <- function(x, time, threshold) {
  v <- diff(x) / diff(time)
  y <- v[abs(v) >= threshold]
  sum(abs(diff(sign(y))) > 1)
}

checkConditionPeriodic <- function(traj, threshold, kthLargest) {
  normalizer <- DEEBtrajs::calculateNormalization(traj)
  trajNormed <- normalizer$normalize(traj)
  dft <- stats::fft(trajNormed$state)
  n <- nrow(dft)
  x <- abs(dft)[1:floor(n/2), ]
  spec <- rowSums(x^2)
  res <- max(spec) / sort(spec, decreasing=TRUE)[floor(kthLargest * log(n))]
  return(unname(res) <= threshold)
}


