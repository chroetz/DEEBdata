sampleUniformOnBall <- function(n, d, rMin, rMax) {
  direction <- matrix(stats::rnorm(d*n), nrow = n)
  direction <- direction / sqrt(rowSums(direction^2))
  radius <- stats::runif(n, min = rMin/rMax, max = 1)^(1/d)
  direction * radius * rMax
}

buildArraySampler <- function(opts, arrayDim) {
  p <- prod(arrayDim)
  if (opts$name == "uniform") {
    sampleBase <- \()
      stats::runif(
        p,
        min = opts$range[1],
        max = opts$range[2]
      )
  } else if (opts$name == "normal") {
    sampleBase <- \()
      stats::rnorm(
        p,
        mean = opts$mean,
        sd = opts$sd
      )
  } else if (opts$name == "const") {
    sampleBase <- \() opts$value
  } else if (opts$name == "uniformOnBall") {
    sampleBase <- \()
      sampleUniformOnBall(
        prod(arrayDim[-1]),
        arrayDim[1],
        opts$range[1],
        opts$range[2]
      )
  } else {
    stop("Unrecognized name ", opts$name)
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



