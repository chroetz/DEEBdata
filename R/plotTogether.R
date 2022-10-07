plotTogether <- function(opts, writeOpts = TRUE) {

  opts <- asOpts(opts, "Plot")
  if (!dir.exists(opts$path)) dir.create(opts$path)
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Plot"))

  plotsPath <- file.path(opts$path, "plots")
  if (!dir.exists(plotsPath)) dir.create(plotsPath)
  plotsPath <- normalizePath(plotsPath, mustWork=TRUE)

  metaObs <- DEEBpath::getMetaGeneric(
    c(opts$truthPath, opts$obsPath),
    tagsFilter = c("truth", "obs"))

  for (i in seq_len(nrow(metaObs))) {
    info <- metaObs[i,]
    plts <- createTruthObsPlots(info)
    for (nm in names(plts)) {
      fileName <- sprintf("Truth%04dObs%04d%s.png", info$truthNr, info$obsNr, nm)
      ggplot2::ggsave(file.path(plotsPath, fileName), plts[[nm]], width = 3, height = 3)
    }
  }

  metaTruth <- DEEBpath::getMetaGeneric(
    c(opts$truthPath, opts$taskPath),
    tagsFilter = c("truth", "task"))
  if (!"taskNr" %in% colnames(metaTruth)) metaTruth <- metaTruth[0,]

  for (i in seq_len(nrow(metaTruth))) {
    info <- metaTruth[i,]
    plts <- createTruthTaskPlots(info)
    for (nm in names(plts)) {
      fileName <- sprintf("Truth%04dTask%02d%s.png", info$truthNr, info$taskNr, nm)
      ggplot2::ggsave(file.path(plotsPath, fileName), plts[[nm]], width = 3, height = 3)
    }
  }

  writeDoc(
    "plots",
    outDir = opts$path,
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
      info$truth, esti = NULL, obs = info$obs, title = title)
    )
}


createTruthTaskPlots <- function(info) {
  info <- DEEBpath::loadPathsInInfo(as.list(info))
  title <- sprintf("Truth %d, Task %d", info$truthNr, info$taskNr)
  taskClass <- getClassAt(info$task, 2)
  switch(
    taskClass,
    "estiObsTrajs" = {
      list(
        stateSpace = DEEBplots::plotStateSpace(
          info$truth, esti = NULL, obs = NULL, title = title),
        timeState = DEEBplots::plotTimeState(
          info$truth, esti = NULL, obs = NULL, title = title)
      )
    },
    "newTrajs" = {
      list(
        stateSpace = DEEBplots::plotStateSpace(
          info$truth, esti = NULL, obs = NULL, title = title),
        timeState = DEEBplots::plotTimeState(
          info$truth, esti = NULL, obs = NULL, title = title)
      )
    },
    "velocity" = {
      list(
        vectorField = DEEBplots::plotVectorField(info$truth, title = title)
      )
    },
    stop("Unknown task class ", taskClass)
  )
}


writeDoc <- function(markdown, outDir, outFile, ...) {
  rmarkdown::render(
    system.file("rmarkdown", paste0(markdown, ".Rmd"), package = "DEEBdata"),
    params = list(...),
    output_dir = outDir,
    output_file= outFile)
}
