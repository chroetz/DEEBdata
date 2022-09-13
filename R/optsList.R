makeOptsList <- function(optsListClass, ..., .lst = NULL) {
  optsList <- c(.lst, list(...))
  optsList <- setOptsListClass(optsList, optsListClass)
  validateOptsList(optsList)
}


validateOptsList <- function(optsList) {
  stopifnot(inherits(optsList, "OptsList"))
  stopifnot(is.list(optsList))
  lapply(optsList, validateOpts)
  return(invisible(optsList))
}

asOptsList <- function(x, optsListClass) {
  optsList <- makeOptsList(optsListClass, .lst = x)
  validateOpts(optsList)
}

setOptsListClass <- function(optsList, optsListClass) {
  class(optsList) <- c(optsListClass, "OptsList")
  return(optsList)
}
