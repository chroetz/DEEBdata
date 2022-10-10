generateObservations <- function(opts, writeOpts = TRUE) {

  opts <- asOpts(opts, "Observation")
  if (!dir.exists(opts$path)) dir.create(opts$path)
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Observation"))

  set.seed(opts$seed)

  meta <- DEEBpath::getMetaGeneric(opts$truthPath, tagsFilter = c("obs", "truth"))

  if (nrow(meta) == 0) {
    message("No truth files found.")
    return(invisible(NULL))
  }
  d <-
    meta$truthPath[1] |>
    readTrajs() |>
    getDim()
  noiseSampler <- buildNoiseSampler(opts$noiseSampler, d = d)

  for (i in seq_len(nrow(meta))) {
    info <- meta[i, ]
    message("Process Truth ", info$truthNr)
    truth <- readTrajs(info$truthPath)
    obsNr <- 0
    for (i in seq_len(opts$reps)) for (s in opts$scales) {
      obsNr <- obsNr+1
      message("Generate observations. Iteration ", obsNr)
      obs <- observeGrid(truth, opts$n, opts$tStep, noiseSampler, scale=s)
      obsFileName <- DEEBpath::obsFile(truthNr = info$truthNr, obsNr = obsNr)
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
