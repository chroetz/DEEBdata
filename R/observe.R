observeGrid <- function(trajs, n, tStep, noiseSampler) {
  tSample <- seq(0, by = tStep, length.out = n)
  obs <- interpolateTrajs(trajs, tSample)
  trajIds <- unique(trajs$trajId)
  for (trajId in trajIds) {
    nTraj <- sum(obs$trajId == trajId)
    obs$state[obs$trajId == trajId, ] <-
      obs$state[obs$trajId == trajId, ] +
      noiseSampler(nTraj)
  }
  return(obs)
}

buildNoiseSampler <- function(opts, d) {
  sampler1 <- buildArraySampler(opts, arrayDim = c(1, d))
  noiseSampler <- function(n) {
    lst <- replicate(n, sampler1(), simplify=FALSE)
    do.call(rbind, lst)
  }
  return(noiseSampler)
}

generateObservations <- function(opts) {

  opts <- asOpts(opts, "Noise")

  writeOpts(opts, file.path(opts$path, "_opts_noise"))

  set.seed(opts$seed)

  files <-
    dir(opts$path) |>
    grep("^truth\\d+\\.csv$", x = _, value=TRUE)

  if (length(files) == 0) {
    message("No truth files found.")
    return(invisible(NULL))
  }
  d <-
    file.path(opts$path, files[1]) |>
    readTrajs() |>
    getDim()
  noiseSampler <- buildNoiseSampler(opts$noiseSampler, d = d)

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
