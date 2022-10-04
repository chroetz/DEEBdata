plotTogether <- function(opts, writeOpts = TRUE) {

  opts <- asOpts(opts, "Plot")
  if (writeOpts) writeOpts(opts, file.path(opts$outPath, "Opts_Plot"))

  plotsPath <- file.path(opts$outPath, "plots")
  if (!dir.exists(plotsPath)) dir.create(plotsPath)
  plotsPath <- normalizePath(plotsPath, mustWork=TRUE)

  metaTruthObs <- DEEBpath::getMetaGeneric(
    c(opts$truthPath, opts$obsPath),
    tagsFilter = c("truth", "obs"))

  metaTruthObs$plots <- lapply(
    seq_len(nrow(metaTruthObs)),
    \(i) createTruthObsPlots(metaTruthObs[i,]))

  for (i in 1:nrow(metaTruthObs)) {
    row <- metaTruthObs[i,]
    plots <- row$plots[[1]]
    for (nm in names(plots)) {
      plt <- plots[[nm]]
      if (length(plt) == 0) next
      fileName <- sprintf(
        "Truth%04dObs%04d_%s.png",
        row$truthNr, row$obsNr, nm)
      ggplot2::ggsave(file.path(plotsPath, fileName), plt, width = 3, height = 3)
    }
  }

  if (file.exists(file.path(opts$truthOptsPath, "Opts_Truth.json"))) {
    truthOpts <- readOptsBare(file.path(opts$truthOptsPath, "Opts_Truth.json"))
  } else {
    truthOpts <- readOptsBare(file.path(opts$truthOptsPath, "Opts_Run.json"))$truthOpts
  }
  fun <- getParmsFunction(truthOpts$deFunSampler)

  metaTruth <- DEEBpath::getMetaGeneric(
    opts$truthPath,
    tagsFilter = c("truth", "_parms"))

  metaTruth$plots <- lapply(
    seq_len(nrow(metaTruth)),
    \(i) createTruthPlots(metaTruth[i,], fun))

  for (i in 1:nrow(metaTruth)) {
    row <- metaTruth[i,]
    plots <- row$plots[[1]]
    for (nm in names(plots)) {
      plt <- plots[[nm]]
      if (length(plt) == 0) next
      fileName <- sprintf(
        "Truth%04d_%s.png",
        row$truthNr, nm)
      ggplot2::ggsave(file.path(plotsPath, fileName), plt, width = 3, height = 3)
    }
  }

  writeDoc(
    "plots",
    outDir = opts$outPath,
    outFile = opts$outFileName,
    plotsDir = plotsPath)
}

createTruthObsPlots <- function(info) {
  info <- DEEBpath::loadPathsInInfo(as.list(info))
  title <- sprintf("Truth %d, Obs %d", info$truthNr, info$obsNr)
  list(
    stateSpace = DEEBplots::plotStateSpace(
      info$truth, esti = NULL, obs = info$obs, title = title),
    timeState = DEEBplots::plotTimeState(
      info$truth, esti=NULL, obs = info$obs, title = title))
}

createTruthPlots <- function(info, fun) {
  info <- DEEBpath::loadPathsInInfo(as.list(info))
  title <- sprintf("Truth %d", info$truthNr)
  list(
    velocityField = DEEBplots::plotVectorField(
      info$truth, fun, info$`_parms`, title = title))
}

writeDoc <- function(markdown, outDir, outFile, ...) {
  rmarkdown::render(
    system.file("rmarkdown", paste0(markdown, ".Rmd"), package = "DEEBdata"),
    params = list(...),
    output_dir = outDir,
    output_file= outFile)
}
