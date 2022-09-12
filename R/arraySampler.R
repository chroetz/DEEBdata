sampleUniformOnBall <- function(n, d, rMin, rMax) {
  direction <- matrix(stats::rnorm(d*n), nrow = n)
  direction <- direction / sqrt(rowSums(direction^2))
  radius <- stats::runif(n, min = rMin/rMax, max = 1)^(1/d)
  direction * radius * rMax
}

buildArraySampler <- function(opts, arrayDim) {
  if (opts$name == "uniform") {
    sampler <- \()
      stats::runif(
        prod(arrayDim),
        min = opts$range[1],
        max = opts$range[2]
      ) |>
        array(arrayDim)
  } else if (opts$name == "normal") {
    sampler <- \()
      stats::rnorm(
        prod(arrayDim),
        mean = opts$mean,
        sd = opts$sd
      ) |>
      array(arrayDim)
  } else if (opts$name == "const") {
    sampler <- \() array(opts$value, arrayDim)
  } else if (opts$name == "uniformOnBall") {
    sampler <- \()
      sampleUniformOnBall(
        prod(arrayDim[-1]),
        arrayDim[1],
        opts$rMin,
        opts$rMax
      ) |>
        array(arrayDim)
  } else {
    stop("Unrecognized name ", opts$name)
  }
  return(sampler)
}


