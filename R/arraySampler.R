sampleUniformOnBall <- function(n, d, rMin, rMax) {
  direction <- matrix(stats::rnorm(d*n), nrow = n)
  direction <- direction / sqrt(rowSums(direction^2))
  radius <- stats::runif(n, min = rMin/rMax, max = 1)^(1/d)
  direction * radius * rMax
}

sampleUniformRadiusBall <- function(n, d, rMin, rMax) {
  direction <- matrix(stats::rnorm(d*n), nrow = n)
  direction <- direction / sqrt(rowSums(direction^2))
  radius <- stats::runif(n, min = rMin/rMax, max = 1)
  direction * radius * rMax
}

sampleOnLorenz63Attractor <- function(n) {
  states <- readRDS(system.file("lorenz63.RDS", package="DEEBdata"))
  states[sample.int(nrow(states), n),]
}

buildArraySampler <- function(opts, arrayDim) {
  p <- prod(arrayDim)
  n <- arrayDim[1]
  d <- prod(arrayDim[-1])
  clss <- getThisClass(opts)
  if (clss == "uniform") {
    sampleBase <- \()
      stats::runif(
        p,
        min = opts$range[1],
        max = opts$range[2]
      )
  } else if (clss == "normal") {
    sampleBase <- \()
      stats::rnorm(
        p,
        mean = opts$mean,
        sd = opts$sd
      )
  } else if (clss == "const") {
    sampleBase <- \() opts$value
  } else if (clss == "uniformOnBall") {
    sampleBase <- \()
      sampleUniformOnBall(n, d, opts$range[1], opts$range[2])
  } else if (clss == "uniformInRect") {
    sampleBase <- \() {
      apply(opts$ranges, 1, \(r) stats::runif(n, min=r[1], max=r[2]))
    }
  } else if (clss == "uniformRadiusBall") {
    sampleBase <- \()
      sampleUniformRadiusBall(n, d, opts$range[1], opts$range[2])
  } else if (clss == "lorenz63") {
    sampleBase <- \() sampleOnLorenz63Attractor(n)
  } else {
    stop("Unrecognized name ", clss)
  }
  sampleArray <- \() array(sampleBase(), arrayDim)
  if (length(opts$sparsity) == 1 && opts$sparsity < p) {
    sampleSupport <- buildArraySupportSampler(opts$sparsity, arrayDim, opts$keepFirstColumn)
    sample <- \() sampleArray() * sampleSupport()
  } else {
    sample <- sampleArray
  }
  return(sample)
}

buildArraySupportSampler <- function(supportSize, arrayDim, keepFirstColumn) {
  p <- prod(arrayDim)
  if (isTRUE(keepFirstColumn)) {
    stopifnot(length(arrayDim) == 2) # only for matrix
    sampleSupport <- \() matrix(
      c(rep(TRUE, arrayDim[1]),
        sample.int(p - arrayDim[1]) <= (supportSize - arrayDim[1])),
      nrow = arrayDim[1])
  } else {
    sampleSupport <- \() array(sample.int(p) <= supportSize, arrayDim)
  }
  return(sampleSupport)
}



