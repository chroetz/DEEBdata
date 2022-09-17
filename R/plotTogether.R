openPlotDevice <- function(opts, mfrow) {
  name <- class(opts)[1]
  if (name == "pdf") {
    message("Opening PDF device file ", opts$outFile)
    grDevices::pdf(
      opts$outFile,
      width = mfrow[2]*opts$scale,
      height = mfrow[1]*opts$scale)
    return(grDevices::dev.off)
  } else if (name == "default") {
    return(function() NULL)
  } else {
    stop("Unrecognized name ", name)
  }
}


plotTogether <- function(opts, writeOpts = TRUE) {

  opts <- asOpts(opts, "Plot")
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Plot"))

  truthParmsFiles <-
    opts$path |>
    dir() |>
    grep("^truth\\d+_parms\\.json$", x = _, value=TRUE)
  truthTrajFiles <-
    opts$path |>
    dir() |>
    grep("^truth\\d+\\.csv$", x = _, value=TRUE)

  # TODO: make sure they match

  len <- length(truthTrajFiles)
  n <- ceiling(sqrt(len))
  mfrow <- c(ceiling(len/n), n)
  finalizeDevice <- openPlotDevice(opts$device, mfrow = mfrow)
  graphics::par(mfrow = mfrow)
  graphics::par(mar = if (opts$axes) c(2,2,2,2) else c(0,0,0,0))

  if (file.exists(file.path(opts$path, "Opts_Truth.json"))) {
    truthOpts <- readOpts(
      file.path(opts$path, "Opts_Truth.json"),
      optsClass = "Truth",
      .fill=FALSE)
  } else {
    truthOpts <- readOpts(
      file.path(opts$path, "Opts_Run.json"),
      optsClass = "Run",
      .fill=FALSE)$truthOpts
  }
  fun <- getParmsFunction(truthOpts$deFunSampler)

  for (i in seq_len(len)) {

    flTraj <- truthTrajFiles[i]
    fullPathTraj <- file.path(opts$path, flTraj)
    trajs <- readTrajs(fullPathTraj)

    nr <- as.integer(stringr::str_match(flTraj, "\\d+"))

    obs <- readTrajs(paste0(
      substr(fullPathTraj, 1, nchar(fullPathTraj)-4),
      sprintf("obs%04d.csv", opts$obsNr)))

    plotTrajAndObs(trajs, obs, title = nr, opts = opts)
  }

  pltList <- lapply(seq_len(len), \(i) {

    flParms <- truthParmsFiles[i]
    fullPathParms <- file.path(opts$path, flParms)
    parms <- readOpts(
      fullPathParms,
      c(class(truthOpts$deFunSampler)[1], "Parms"),
      .fill = FALSE)
    flTraj <- truthTrajFiles[i]
    fullPathTraj <- file.path(opts$path, flTraj)
    traj <- readTrajs(fullPathTraj)

    nr <- as.integer(stringr::str_match(flTraj, "\\d+"))

    plotVectorField(traj, fun, parms, title = nr, axes = opts$axes)
  })

  plts <- gridExtra::arrangeGrob(grobs = pltList, ncol = n)
  plot(plts)

  finalizeDevice()
}

