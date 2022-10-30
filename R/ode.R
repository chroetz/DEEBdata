solveOde <- function(fun, u0, timeRange, opts, parms = NULL) {
  opts <- asOpts(opts, "OdeSolver")
  tm <- seq(timeRange[1], timeRange[2], by = opts$timeStep)
  .solveOde(
    fun, u0, tm, parms,
    method = opts$method,
    additionalArgs = opts$additonalArgs)
}

solveOdeMulti <- function(fun, u0, timeRange, opts, parms = NULL) {
  opts <- asOpts(opts, "OdeSolver")
  tm <- seq(timeRange[1], timeRange[2], by = opts$timeStep)
  if (is.null(nrow(u0))) {
    u0 <- matrix(u0, nrow=1)
  }
  if (is.null(rownames(u0))) {
    rownames(u0) <- seq_len(nrow(u0))
  }
  trajIds <- rownames(u0)
  trajList <- lapply(trajIds, \(id) {
    trajs <- .solveOde(
      fun, u0[id, ], tm, parms,
      method = opts$method,
      additionalArgs = opts$additonalArgs)
    if (is.null(trajs)) {
      stop("Cannot solve this ODE")
    }
    trajs <- setTrajId(trajs, id)
    return(trajs)
  })
  return(bindTrajs(trajList))
}

.solveOde <- function(fun, u0, times, parms, method, additionalArgs) {
  u <- do.call(
      deSolve::ode,
      c(
        list(
          y = u0,
          times = times,
          func = fun,
          parms = parms,
          method = method),
        additionalArgs
      )
    )
  if (any(!is.finite(u))) return(NULL)
  colnames(u) <- c("time", paste0("state", seq_len(ncol(u)-1)))
  return(asTrajs(u))
}
