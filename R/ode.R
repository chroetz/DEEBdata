solveOde <- function(fun, u0, tMax, tStep, opts = list(), parms = NULL) {
  tm <- seq(0, tMax, by = tStep)
  suppressWarnings(suppressMessages(utils::capture.output( # make silent
    u <- do.call(
      deSolve::ode,
      c(list(y = u0, times = tm, func = fun, parms = parms), opts))
  )))
  return(u)
}
