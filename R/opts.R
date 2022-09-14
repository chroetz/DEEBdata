makeOpts <- function(optsClass, ..., .lst = NULL, .fill = TRUE) {
  opts <- c(.lst, list(...))
  opts <- setOptsClass(opts, optsClass)
  if (.fill) opts <- fillWithDefaultOpts(opts)
  validateOpts(opts)
}

# keep attributes
`[.Opts` <- function(x, i, ...) {
  a <- attributes(x)
  attributes(x) <- NULL
  res <- x[i, ...]
  a$names <- a$names[i]
  attributes(res) <- a
  return(res)
}


validateOpts <- function(opts) {
  stopifnot(inherits(opts, "Opts"))
  stopifnot(length(oldClass(opts)) >= 2)
  stopifnot(is.list(opts))
  stopifnot(!is.null(names(opts)) || length(opts) == 0)
  stopifnot(length(unique(names(opts))) == length(opts))
  # TODO: Compare with default opts. validate sub classes. Check names and entries from default.
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

fillWithDefaultOpts <- function(opts, optsClass = NULL, onlySubOpts = FALSE) {
  if (is.null(optsClass)) {
    stopifnot(inherits(opts, "Opts"), length(oldClass(opts)) > 1)
  } else {
    opts <- setOptsClass(opts, optsClass)
  }
  optsClass <- oldClass(opts)[1]
  if (!onlySubOpts) {
    fl <- system.file("defaultOpts", paste0(optsClass, ".json"), package = "DEEData")
    if (!file.exists(fl)) {
      stop("Cannot find defaultOpts for class: ", optsClass)
    }
    defaultOpts <- readOpts(
      fl,
      optsClass = optsClass,
      .fill = FALSE,
      removeUnderscoreEntries = FALSE)
    opts <- fillOpts(opts, defaultOpts)
  }
  for (i in seq_along(opts)) {
    if (inherits(opts[[i]], "Opts")) {
      opts[[i]] <- fillWithDefaultOpts(opts[[i]])
    } else if (inherits(opts[[i]], "OptsList")) {
      for (j in seq_along(opts[[i]])) {
        opts[[i]][[j]] <- fillWithDefaultOpts(opts[[i]][[j]])
      }
    }
  }
  return(opts)
}

fillOpts <- function(opts, filler) {
  if (!"name" %in% names(opts) && "name" %in% names(filler)) {
    opts$name <- filler$name
  }
  allEntries <- names(filler)
  allOptsEntries <- allEntries[!startsWith(allEntries, "_")]
  if ("name" %in% names(opts)) {
    optsName <- opts$name
    suffix <- "_used_by"
    usedByEntries <- allEntries[startsWith(allEntries, "_") & endsWith(allEntries, suffix)]
    if (length(usedByEntries) == 0) {
      fillableEntries <- allOptsEntries
    } else {
      usedByOptsEntries <- substr(usedByEntries, start = 2, stop = nchar(usedByEntries) - nchar(suffix))
      unrestrictedEntries <- allOptsEntries[!allOptsEntries %in% usedByOptsEntries]
      restrictedEntries <- allOptsEntries[allOptsEntries %in% usedByOptsEntries]
      used <- sapply(restrictedEntries, \(nm) optsName %in% filler[[paste0("_", nm, suffix)]])
      fillableEntries <- c(unrestrictedEntries, restrictedEntries[used])
    }
  } else {
    fillableEntries <- allOptsEntries
  }

  for (nm in fillableEntries) {
    if (nm %in% names(opts)) next
    opts[[nm]] <- filler[[nm]]
  }
  return(opts)
}

checkOptsHasDefault <- function(opts, optsClass = NULL) {
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
  unknownNames <- setdiff(names(opts), names(defaultOpts))
  lapply(unknownNames, \(nm) cat("Opts entry unknown: ", nm, "\n"))
  for (i in seq_along(opts)) {
    if (inherits(opts[[i]], "Opts")) {
      cat("Checking Opts ", names(opts)[i], "\n")
      checkOptsHasDefault(opts[[i]])
    } else if (inherits(opts[[i]], "OptsList")) {
      cat("Checking OptsList ", names(opts)[i], "\n")
      for (j in seq_along(opts[[i]])) {
        checkOptsHasDefault(opts[[i]][[j]])
      }
    }
  }
  return(invisible(NULL))
}
