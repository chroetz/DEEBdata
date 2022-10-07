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
          tStep = opts$tStepOde,
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


sampleTrajectoriesAndWriteForTasks <- function(opts, taskList, observationOpts, writeOpts = TRUE) {

  opts <- asOpts(opts, "Truth")
  if (!dir.exists(opts$path)) dir.create(opts$path)
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Truth"))

  parmsSampler <- buildParmsSampler(opts$deFunSampler)
  fun <- getParmsFunction(opts$deFunSampler)
  u0Sampler <- buildArraySampler(opts$u0Sampler, arrayDim = c(1, opts$deFunSampler$d))

  set.seed(opts$seed)

  for (truthNr in seq_len(opts$reps)) {
    message("Iteration ", truthNr, " of ", opts$reps, ".")
    res <- sampleConditional(parmsSampler, fun, u0Sampler, opts)
    writeOpts(res$parms, file.path(opts$path, sprintf("truth%04dparms",truthNr)))
    writeTruthForObservation(
      res$trajs,
      observationOpts,
      file.path(opts$path, sprintf("truth%04d.csv", truthNr)))
    for (taskNr in seq_along(taskList$list)) {
      writeTurthForTask(
        res$trajs, res$parms, fun,
        taskList$list[[taskNr]],
        file.path(opts$path, sprintf("task%02dtruth%04d.csv", taskNr, truthNr)),
        opts)
    }
  }
}

writeTruthForObservation <- function(trajs, observationOpts, filePath) {
  times <- seq(0, by = observationOpts$tStep, length.out = observationOpts$n)
  outTrajs <- interpolateTrajs(trajs, times)
  writeTrajs(outTrajs, filePath)
}

writeTurthForTask <- function(trajs, parms, derivFun, task, filePath, opts) {
  taskClass <- getClassAt(task, 2)
  switch(
    taskClass,
    "estiObsTrajs" = {
      times <- seq(task$predictionTime[1], task$predictionTime[2], task$timeStep)
      outTrajs <- interpolateTrajs(trajs, times)
      writeTrajs(outTrajs, filePath)
    },
    "newTrajs" = {
      newTrajs <- solveOdeMulti(
        derivFun, task$initialState,
        tMax = task$predictionTime[2],
        tStep = opts$tStepOde,
        opts = opts$odeSolver,
        parms = parms)
      times <- seq(task$predictionTime[1], task$predictionTime[2], task$timeStep)
      outTrajs <- interpolateTrajs(newTrajs, times)
      writeTrajs(outTrajs, filePath)
    },
    "velocity " = {
      gridSides <- lapply(seq_along(task$gridSteps), \(i) seq(
        task$gridRanges[i,1],
        task$gridRanges[i,2],
        task$gridSteps[i]
      ))
      states <- as.matrix(expand.grid(gridSides))
      derivs <- t(apply(states, 1, \(s) derivFun(0, s, parms)[[1]]))
      result <- makeDerivTrajs(state = states, deriv = derivs)
      writeDerivTrajs(result, filePath)
    },
    stop("Unknown task class ", taskClass))
}
