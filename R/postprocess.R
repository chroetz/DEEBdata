composeWithPostprocessors <- function(fun, opts) {
  postprocessors <- lapply(opts$list, buildPostprocessor)
  funWithPostprocessing <- function(t, u, parms) {
    du <- fun(t, u, parms)
    for (pp in postprocessors) {
      du <- pp(du, u, t)
    }
    res <- formatForSolver(du)
    return(res)
  }
  return(funWithPostprocessing)
}

formatForSolver <- function(du) {
  du <- as.vector(du)
  return(list(du))
}

buildPostprocessor <- function(opts) {
  name <- getThisClass(opts)
  postproc <- switch(
    name,
    identity = function(du, u, t) du,
    softBoundary = getPostprocessorSoftBoundary(opts),
    stop("Unrecognized name ", name))
  return(postproc)
}

getPostprocessorSoftBoundary <- function(opts) {
  rInner <- opts$boundaryRange[1]
  rOuter <- opts$boundaryRange[2]
  len <- diff(opts$boundaryRange)
  postprocessor <- function(du, u, t) {
    uNorm <- sqrt(sum(u^2))
    if (uNorm > rOuter) {
      du <- -u / uNorm
    } else if (uNorm > rInner) {
      du <- (-u / uNorm * (uNorm - rInner) + du * (rOuter - uNorm)) / len
    }
    return(du)
  }
  return(postprocessor)
}
