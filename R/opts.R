makeOpts <- function(optsClass, ..., .lst = NULL, .fill = TRUE) {
  opts <- c(.lst, list(...))
  opts <- setOptsClass(opts, optsClass)
  if (.fill) opts <- fillWithDefaultOpts(opts)
  validateOpts(opts)
}

validateOpts <- function(opts) {
  stopifnot(inherits(opts, "Opts"))
  stopifnot(is.list(opts))
  # TODO: validate sub classes
  return(invisible(opts))
}

asOpts <- function(x, optsClass, .fill = TRUE) {
  if (length(x) == 1 && is.character(x) && file.exists(x)) {
    opts <- readOpts(x, optsClass, .fill = .fill)
  } else {
    opts <- makeOpts(optsClass, .lst = x, .fill = .fill)
  }
  validateOpts(opts)
}

setOptsClass <- function(opts, optsClass) {
  class(opts) <- c(optsClass, "Opts")
  return(opts)
}

fillWithDefaultOpts <- function(opts, optsClass = NULL) {
  if (is.null(optsClass)) {
    stopifnot(inherits(opts, "Opts"), length(oldClass(opts)) > 1)
  } else {
    opts <- setOptsClass(opts, optsClass)
  }
  optsClass <- oldClass(opts)[1]
  fl <- system.file("defaultOpts", paste0(optsClass, ".json"), package = "DEEData")
  if (!file.exists(fl)) {
    stop("Cannot find defaultOpts for class: ", optsClass)
  }
  defaultOpts <- readOpts(
    fl,
    optsClass = optsClass,
    .fill = FALSE)
  opts <- fillOpts(opts, defaultOpts)
  if (length(opts) > 0) {
    isOpts <- sapply(opts, inherits, "Opts")
    opts[isOpts] <- lapply(opts[isOpts], fillWithDefaultOpts)
  }
  return(opts)
}

fillOpts <- function(opts, filler) {
  for (nm in names(filler)) {
    if (nm %in% names(opts)) next
    opts[[nm]] <- filler[[nm]]
  }
  return(opts)
}
