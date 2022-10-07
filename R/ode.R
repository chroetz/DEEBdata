solveOde <- function(fun, u0, tMax, tStep, opts = list(), parms = NULL) {
  opts <- asOpts(opts, "OdeSolver")
  tm <- seq(0, tMax, by = tStep)
  suppressWarnings(suppressMessages(utils::capture.output( # make silent
    u <- do.call(
      deSolve::ode,
      c(list(y = u0, times = tm, func = fun, parms = parms), opts))
  )))
  colnames(u) <- c("time", paste0("state", seq_len(ncol(u)-1)))
  return(asTrajs(u))
}

solveOdeMulti <- function(fun, initialStates, tMax, tStep, opts = list(), parms = NULL) {
  # TODO: fix code duplication; strange handling of time; move to DEEBtrajs package
  opts <- asOpts(opts, "OdeSolver")
  tm <- seq(0, tMax, by = tStep)
  if (is.null(nrow(u0))) {
    u0 <- matrix(u0, nrow=1)
  }
  if (is.null(rownames(u0))) {
    rownames(u0) <- seq_len(nrow(u0))
  }
  trajIds <- rownames(u0)
  trajList <- lapply(seq_len(nrow(u0)), \(i) {
    suppressWarnings(suppressMessages(utils::capture.output( # make silent
      u <- do.call(
        deSolve::ode,
        c(list(y = u0[i, ], times = tm, func = fun, parms = parms), opts))
    )))
    colnames(u) <- c("time", paste0("state", seq_len(ncol(u)-1)))
    trajs <- asTrajs(u)
    trajs <- setTrajId(trajs, trajIds[i])
    return(trajs)
  })
  return(bindTrajs(trajList))
}
