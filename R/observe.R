observeGrid <- function(u, n, tStep, noiseSampler) {
  d <- ncol(u) - 1
  tSample <- seq(0, by = tStep, length.out = n)
  truth <- sapply(1 + 1:d, \(j) stats::approx(u[,1], u[,j], tSample)$y)
  cbind(tSample, truth + noiseSampler(n, d))
}

buildNoiseSampler <- function(opts) {
  if (opts$name == "normal") {
    noiseSampler <- \(n, d) matrix(stats::rnorm(n*d, sd = opts$sd), nrow = n)
  } else {
    stop("Unrecognized name ", opts$name)
  }
  return(noiseSampler)
}

generateObservations <- function(opts) {

  opts <- readOpts(opts)

  writeOpts(opts, file.path(opts$path, "_opts_noise"))

  set.seed(opts$seed)

  files <-
    dir(opts$path) |>
    grep("^truth\\d+\\.csv$", x = _, value=TRUE)

  noiseSampler <- buildNoiseSampler(opts$noiseSampler)

  for (fl in files) {
    message("Process Truth in ", fl)
    fullPath <- file.path(opts$path, fl)
    truth <- readDeData(fullPath)

    for (i in seq_len(opts$reps)) {
      message("Generate observations. Iteration ", i)
      obs <- observeGrid(truth, opts$n, opts$tStep, noiseSampler)
      writeDeData(
        obs,
        file.path(opts$path, paste0(substr(fl, 1, nchar(fl)-4), sprintf("obs%04d.csv", i))))
    }
  }
}
