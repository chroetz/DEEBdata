generateObservations <- function(opts, writeOpts = TRUE) {

  message("Creating Observations")

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
      obs <- observeGrid(
        truth,
        opts$n, opts$timeStep, opts$timeLimit,
        opts$random,
        noiseSampler,
        scale=s,
        noiseFree = opts$noiseFree)
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

observeGrid <- function(
    truthTrajs,
    n, timeStep, timeLimit,
    random,
    noiseSampler,
    scale=1,
    noiseFree=NULL
  ) {

  obsTrajs <- mapTrajs2Trajs(truthTrajs, \(truthTraj) {

    noiseFreeRows <- unique(c(
      noiseFree[noiseFree>0],
      (nrow(truthTraj)+1) + noiseFree[noiseFree<0]))
    noise <- scale * noiseSampler(nrow(truthTraj))
    noise[noiseFreeRows,] <- 0
    obsTraj <- makeTrajs(truthTraj$time, truthTraj$state + noise, trajId=truthTraj$trajId)

    return(obsTraj)
  })

  return(obsTrajs)
}


getTimes <- function(n, timeLimit, timeStep, random) {
  if (random) {
    timeSteps <- stats::rexp(
      pmin(n, ceiling(timeLimit / timeStep)*2+1000),
      rate = 1 / timeStep)
    stopifnot(sum(timeSteps) > timeLimit)
    tSample <- c(0, cumsum(timeSteps))
  } else {
    tSample <- seq(0, by = timeStep, length.out = n)
  }
  tSample <- tSample[(tSample < timeLimit) & (seq_along(tSample) <= n)]
  return(tSample)
}
