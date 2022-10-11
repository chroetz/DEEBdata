solveOde <- function(fun, u0, tMax, tStep, opts = list(), parms = NULL) {
  opts <- asOpts(opts, "OdeSolver")
  tm <- seq(0, tMax, by = tStep)
  .solveOde(fun, u0, tm, parms, opts)
}

solveOdeMulti <- function(fun, u0, tMax, tStep, opts = list(), parms = NULL) {
  opts <- asOpts(opts, "OdeSolver")
  tm <- seq(0, tMax, by = tStep)
  if (is.null(nrow(u0))) {
    u0 <- matrix(u0, nrow=1)
  }
  if (is.null(rownames(u0))) {
    rownames(u0) <- seq_len(nrow(u0))
  }
  trajIds <- rownames(u0)
  trajList <- lapply(trajIds, \(id) {
    trajs <- .solveOde(fun, u0[id, ], tm, parms, opts)
    trajs <- setTrajId(trajs, id)
    return(trajs)
  })
  return(bindTrajs(trajList))
}

.solveOde <- function(fun, u0, time, parms, opts) {
  suppressWarnings(suppressMessages(utils::capture.output( # make silent
    u <- do.call(
      deSolve::ode,
      c(list(y = u0, times = time, func = fun, parms = parms), opts))
  )))
  colnames(u) <- c("time", paste0("state", seq_len(ncol(u)-1)))
  if (any(!is.finite(u))) return(NULL)
  return(asTrajs(u))
}
