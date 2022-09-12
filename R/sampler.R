sampleUniformOnBall <- function(n, d, rMin, rMax) {
  direction <- matrix(stats::rnorm(d*n), nrow = n)
  direction <- direction / sqrt(rowSums(direction^2))
  radius <- stats::runif(n, min = rMin/rMax, max = 1)^(1/d)
  direction * radius * rMax
}

sampleUniformOnHypercube <- function(n, d, range) {
  matrix(stats::runif(d*n, min = range[1], max = range[2]), nrow = n)
}

buildSingleSampler <- function(opts) {
  if (opts$name == "uniformBall") {
    sampler <- \() sampleUniformOnBall(1, opts$d, opts$rMin, opts$rMax)
  } else if (opts$name == "hypercube") {
    sampler <- \() sampleUniformOnHypercube(1, opts$d, opts$range)
  } else if (opts$name == "const") {
    sampler <- \() opts$value
  } else {
    stop("Unrecognized name ", opts$name)
  }
  return(sampler)
}

buildMatrixSampler <- function(opts) {
  if (opts$name == "uniformBall") {
    sampler <- \(n, d) sampleUniformOnBall(n, d, opts$rMin, opts$rMax)
  } else if (opts$name == "hypercube") {
    sampler <- \(n, d) sampleUniformOnHypercube(n, d, opts$range)
  } else if (opts$name == "normal") {
    sampler <- \(n, d) matrix(stats::rnorm(d*n, mean = opts$mean, sd = opts$sd), nrow = n)
  } else {
    stop("Unrecognized name ", opts$name)
  }
  return(sampler)
}

buildMultiSampler <- function(opts) {
  if (opts$name == "uniform") {
    sampler <- \(n) stats::runif(n, min=opts$range[1], max=opts$range[2])
  } else if (opts$name == "normal") {
    sampler <- \(n) stats::rnorm(n, mean = opts$mean, sd = opts$sd)
  } else {
    stop("Unrecognized name ", opts$name)
  }
  return(sampler)
}

