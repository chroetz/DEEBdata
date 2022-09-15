observeGrid <- function(trajs, n, tStep, noiseSampler) {
  d <- ncol(trajs$state)
  trajIds <- unique(trajs$trajId)
  tSample <- seq(0, by = tStep, length.out = n)
  observations <- lapply(trajIds, \(id) {
    sel <- trajs$trajId == id
    truth <- sapply(
      seq_len(d),
      \(j) stats::approx(trajs$time[sel], trajs$state[sel,j], tSample)$y)
    Trajs(
      trajId = id,
      time = tSample,
      state = truth + noiseSampler())
  })
  dplyr::bind_rows(observations)
}

buildNoiseSampler <- function(opts, n, d) {
  buildArraySampler(opts, arrayDim = c(n, d))
}

generateObservations <- function(opts) {

  opts <- asOpts(opts, "NoiseOpts")

  writeOpts(opts, file.path(opts$path, "_opts_noise"))

  set.seed(opts$seed)

  files <-
    dir(opts$path) |>
    grep("^truth\\d+\\.csv$", x = _, value=TRUE)

  if (length(files) == 0) {
    message("No truth files found.")
    return(invisible(NULL))
  }
  d <- readTrajs(file.path(opts$path, files[1]))$state |> ncol()
  noiseSampler <- buildNoiseSampler(opts$noiseSampler, n = opts$n, d = d)

  for (fl in files) {
    message("Process Truth in ", fl)
    fullPath <- file.path(opts$path, fl)
    truth <- readTrajs(fullPath)

    for (i in seq_len(opts$reps)) {
      message("Generate observations. Iteration ", i)
      obs <- observeGrid(truth, opts$n, opts$tStep, noiseSampler)
      writeTrajs(
        obs,
        file.path(opts$path, paste0(substr(fl, 1, nchar(fl)-4), sprintf("obs%04d.csv", i))))
    }
  }
}
