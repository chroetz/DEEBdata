observeGrid <- function(trajs, n, tStep, noiseSampler) {
  d <- ncol(trajs$state)
  trajIds <- unique(trajs$trajId)
  tSample <- seq(0, by = tStep, length.out = n)
  observations <- lapply(trajIds, \(id) {
    sel <- trajs$trajId == id
    truth <- sapply(
      seq_len(d),
      \(j) stats::approx(trajs$time[sel], trajs$state[sel,j], tSample)$y)
    tibble::tibble(
      trajId = id,
      time = tSample,
      state = truth + noiseSampler(n, d))
  })
  dplyr::bind_rows(observations)
}

buildNoiseSampler <- function(opts) {
  # TODO noise sampler vs array sampler
  if (opts$name == "normal") {
    noiseSampler <- \(n, d) matrix(stats::rnorm(n*d, sd = opts$sd), nrow = n)
  } else {
    stop("Unrecognized name ", opts$name)
  }
  return(noiseSampler)
}

generateObservations <- function(opts) {

  opts <- asOpts(opts, "NoiseOpts")

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
