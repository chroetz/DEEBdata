sampleConditional <- function(parmsSampler, fun, u0Sampler, opts) {

  successFun <- FALSE
  nRejections <- 0
  for (i in seq_len(opts$maxRejectionsFun)) {
    parms <- parmsSampler()
    trajList <- list()
    for (k in seq_len(opts$nTrajectories)) {
      successU0 <- FALSE
      for (j in seq_len(opts$maxRejectionsU0)) {
        u0 <- as.vector(u0Sampler())
        traj <- solveOde(
          fun, u0,
          tMax = opts$tMax,
          tStep = opts$tStep,
          opts = opts$odeSolver,
          parms = parms)
        if (checkConditions(opts$conditions, traj, fun, parms)) {
          successU0 <- TRUE
          cat("o")
          break
        }
        nRejections <- nRejections + 1
        cat("x")
      }
      if (!successU0) {
        break
      }
      trajList[[k]] <- setTrajId(traj, k)
    }
    if (length(trajList) == opts$nTrajectories) {
      successFun <- TRUE
      cat("\n")
      break
    }
  }
  if (!successFun) stop("Could not meet conditions.")
  message("Created ", length(trajList), " trajectories. ",
          nRejections, " rejections in the process.")

  return(list(trajs = bindTrajs(trajList), parms = parms))
}


sampleTrajectories <- function(opts, writeOpts = TRUE) {

  opts <- asOpts(opts, "Truth")
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Truth"))

  parmsSampler <- buildParmsSampler(opts$deFunSampler)
  fun <- getParmsFunction(opts$deFunSampler)
  u0Sampler <- buildArraySampler(opts$u0Sampler, arrayDim = opts$deFunSampler$d)

  set.seed(opts$seed)

  for (i in seq_len(opts$reps)) {
    message("Iteration ", i, " of ", opts$reps, ".")
    res <- sampleConditional(parmsSampler, fun, u0Sampler, opts)
    writeTrajs(res$trajs, file.path(opts$path, sprintf("truth%04d.csv",i)))
    writeOpts(res$parms, file.path(opts$path, sprintf("truth%04d_parms",i)))
  }
}
