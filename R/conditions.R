
checkConditionStopped <- function(u, f, parms, thershold) {
  duLast <- unlist(f(u[nrow(u),1], u[nrow(u),-1], parms))
  duLastNorm <- sqrt(sum(duLast^2))
  return(duLastNorm >= thershold)
}

checkCondition <- function(opts, u, f, parms) {
  if (opts$name == "stopped") {
    return(checkConditionStopped(u, f, parms, opts$threshold))
  } else {
    stop("Unrecognized name ", opts$name)
  }
}

checkConditions <- function(opts, u, f, parms) {
  if (length(opts) == 0) return(TRUE)
  all(sapply(opts, checkCondition, u=u, f=f, parms=parms))
}
