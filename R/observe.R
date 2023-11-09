generateObservations <- function(opts, writeOpts = TRUE) {

  message("Creating Observations")

  opts <- asOpts(opts, c("Observation", "List"))
  if (!dir.exists(opts$path)) dir.create(opts$path)
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_List_Observation"))

  set.seed(opts$seed)

  meta <- DEEBpath::getMetaGeneric(opts$truthPath, tagsFilter = c("truth"))

  if (nrow(meta) == 0) {
    message("No truth files found.")
    return(invisible(NULL))
  }

  d <-
    meta$truthPath[1] |>
    readRDS() |>
    getDim()

  for (observerNr in seq_along(opts$list)) {

    message("Generate observation ", observerNr)
    obsOpts <- opts$list[[observerNr]]
    noiseSampler <- buildNoiseSampler(obsOpts$noiseSampler, d = d)

    for (i in seq_len(nrow(meta))) {
      info <- meta[i, ]
      message("Process Truth ", info$truthNr)
      truth <- readRDS(info$truthPath)

      obsTruth <- writeAndGetTruthForObservation(
        truth,
        obsOpts,
        file.path(opts$truthPath, DEEBpath::obsTruthFile(truthNr = info$truthNr, obsNr = observerNr)))

      obs <- observeGrid(
        obsTruth,
        obsOpts$n, obsOpts$timeStep, obsOpts$timeLimit,
        obsOpts$random,
        noiseSampler,
        noiseFree = obsOpts$noiseFree)
      obsFileName <- DEEBpath::obsFile(truthNr = info$truthNr, obsNr = observerNr)
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
