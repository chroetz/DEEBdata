openPlotDevice <- function(opts, mfrow) {
  if (opts$name == "pdf") {
    message("Opening PDF device file ", opts$outFile)
    grDevices::pdf(
      opts$outFile,
      width = mfrow[2]*opts$scale,
      height = mfrow[1]*opts$scale)
    return(grDevices::dev.off)
  } else if (opts$name == "default") {
    return(function() NULL)
  } else {
    stop("Unrecognized name ", opts$name)
  }
}


plotTogether <- function(opts) {
  opts <- readOpts(opts)
  writeOpts(opts, file.path(opts$path, "_opts_plot"))

  truthParmsFiles <-
    opts$path |>
    dir() |>
    grep("^truth\\d+_meta\\.json$", x = _, value=TRUE)
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

  funOpts <- readOpts(file.path(opts$path, "_opts_truth.json"))
  fun <- getParmsFunction(funOpts)

  for (i in seq_len(len)) {

    flTraj <- truthTrajFiles[i]
    fullPathTraj <- file.path(opts$path, flTraj)
    traj <- readDeData(fullPathTraj)

    nr <- as.integer(stringr::str_match(flTraj, "\\d+"))

    obs <- readDeData(paste0(
      substr(fullPathTraj, 1, nchar(fullPathTraj)-4),
      sprintf("obs%04d.csv", opts$obsNr)))

    plotTrajAndObs(traj, obs, title = nr, opts = opts)
  }

  pltList <- lapply(seq_len(len), \(i) {

    flParms <- truthParmsFiles[i]
    fullPathParms <- file.path(opts$path, flParms)
    parms <- readOpts(fullPathParms)
    flTraj <- truthTrajFiles[i]
    fullPathTraj <- file.path(opts$path, flTraj)
    traj <- readDeData(fullPathTraj)

    nr <- as.integer(stringr::str_match(flTraj, "\\d+"))

    plotVectorField(traj, fun, parms, title = nr, axes = opts$axes)
  })

  plts <- gridExtra::arrangeGrob(grobs = pltList, ncol = n)
  plot(plts)

  finalizeDevice()
}

