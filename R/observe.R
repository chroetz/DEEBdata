generateObservations <- function(opts, writeOpts = TRUE) {

  opts <- asOpts(opts, "Observation")
  if (!dir.exists(opts$path)) dir.create(opts$path)
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Observation"))

  set.seed(opts$seed)

  files <-
    dir(opts$truthPath) |>
    grep("^truth\\d+\\.csv$", x = _, value=TRUE)

  if (length(files) == 0) {
    message("No truth files found.")
    return(invisible(NULL))
  }
  d <-
    file.path(opts$truthPath, files[1]) |>
    readTrajs() |>
    getDim()
  noiseSampler <- buildNoiseSampler(opts$noiseSampler, d = d)

  for (fl in files) {
    message("Process Truth in ", fl)
    fullPath <- file.path(opts$truthPath, fl)
    truth <- readTrajs(fullPath)

    z <- 0
    for (i in seq_len(opts$reps)) for (s in opts$scales) {
      z <- z+1
      message("Generate observations. Iteration ", z)
      obs <- observeGrid(truth, opts$n, opts$tStep, noiseSampler, scale=s)
      obsFileName <- paste0(
        substr(fl, 1, nchar(fl)-4), # remove file ending
        sprintf("obs%04d.csv", z))
      writeTrajs(obs, file.path(opts$path, obsFileName))
    }
  }
}


buildNoiseSampler <- function(opts, d) {
  sampler1 <- buildArraySampler(opts, arrayDim = c(1, d))
  noiseSampler <- function(n) {
    lst <- replicate(n, sampler1(), simplify=FALSE)
    do.call(rbind, lst)
  }
  return(noiseSampler)
}


observeGrid <- function(trajs, n, tStep, noiseSampler, scale=1) {
  tSample <- seq(0, by = tStep, length.out = n)
  obs <- interpolateTrajs(trajs, tSample)
  trajIds <- unique(trajs$trajId)
  for (trajId in trajIds) {
    nTraj <- sum(obs$trajId == trajId)
    obs$state[obs$trajId == trajId, ] <-
      obs$state[obs$trajId == trajId, ] +
      scale * noiseSampler(nTraj)
  }
  return(obs)
}
