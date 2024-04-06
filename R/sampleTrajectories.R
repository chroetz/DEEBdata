

sampleTrajectoriesAndWriteForTasks <- function(opts, taskList, observationOpts, writeOpts = TRUE) {

  message("Creating Truth")

  opts <- asOpts(opts, "Truth")
  if (!dir.exists(opts$path)) dir.create(opts$path)
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Truth"))

  parmsSampler <- buildParmsSampler(opts$deFunSampler)
  fun <- getParmsFunction(opts$deFunSampler)
  u0Sampler <- buildArraySampler(opts$u0Sampler, arrayDim = c(1, opts$deFunSampler$d))

  # Some tasks require the existence / calculatability of further trajectories
  # (starting from new initial states) additional to those used for creating the
  # observations and solving the task of predicting these observed trajectories
  # into the future. These are collected here and are then calculated in
  # sampleConditional(). If the ODE solver cannot calculate them, the sampled
  # parms are rejected.
  taskTrajsSettings <- collectRequiredTrajsFromTasks(taskList)

  set.seed(opts$seed)

  for (truthNr in seq_len(opts$reps)) {
    message("Iteration ", truthNr, " of ", opts$reps, ".")
    res <- sampleConditional(parmsSampler, fun, u0Sampler, opts, taskTrajsSettings)
    writeOpts(res$parms, file.path(opts$path, DEEBpath::parmsFile(truthNr = truthNr)))
    saveRDS(res$trajs, file.path(opts$path, DEEBpath::truthFile(truthNr = truthNr)))
    for (taskNr in seq_along(taskList$list)) {
      writeTurthForTask(
        res$trajs, res$parms, fun,
        taskList$list[[taskNr]],
        res$taskTrajs[[taskNr]],
        file.path(opts$path, DEEBpath::taskTruthFile(truthNr = truthNr, taskNr = taskNr)),
        opts)
    }
  }
}


sampleConditional <- function(parmsSampler, fun, u0Sampler, opts, taskTrajsSettings) {

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
          timeRange = opts$timeRange,
          opts = opts$odeSolver,
          parms = parms)
        if (
          !is.null(traj) &&
          checkConditions(opts$conditions, traj, fun, parms)
        ) {
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
    if (length(trajList) < opts$nTrajectories) next

    successTask <- TRUE
    taskTrajs <- lapply(taskTrajsSettings, \(tts) {
      if (length(tts) == 0) return(NULL)
      tt <- lapply(seq_len(nrow(tts$initialState)), \(id) {
        trajs <- solveOde(
          fun, tts$initialState[id, ],
          timeRange = opts$timeRange,
          opts = opts$odeSolver,
          parms = parms)
        if (is.null(trajs)) return(NULL)
        trajs <- setTrajId(trajs, id)
        return(trajs)
      })
      if (any(vapply(tt, is.null, logical(1)))) {
        successTask <<- FALSE
        return(NULL)
      }
      bindTrajs(tt)
    })
    if (!successTask) {
      nRejections <- nRejections + 1
      cat("T")
      next
    }

    successFun <- TRUE
    cat("\n")
    break
  }
  if (!successFun) stop("Could not meet conditions.")
  message("Created ", length(trajList), " trajectories. ",
          nRejections, " rejections in the process.")

  return(list(
    trajs = bindTrajs(trajList),
    parms = parms,
    taskTrajs = taskTrajs))
}

writeAndGetTruthForObservation <- function(trajs, observationOpts, filePath) {
  times <- getTimes(
    observationOpts$n,
    observationOpts$timeLimit,
    observationOpts$timeStep,
    observationOpts$random)
  outTrajs <- interpolateTrajs(trajs, times)
  writeTrajs(outTrajs, filePath)
  return(outTrajs)
}

writeTurthForTask <- function(trajs, parms, derivFun, task, taskTrajs, filePath, opts) {
  taskClass <- getClassAt(task, 2)
  switch(
    taskClass,
    "estiObsTrajs" = {
      times <- seq(task$predictionTime[1], task$predictionTime[2], task$timeStep)
      outTrajs <- interpolateTrajs(trajs, times)
      writeTrajs(outTrajs, filePath)
    },
    "newTrajs" = {
      times <- seq(task$predictionTime[1], task$predictionTime[2], task$timeStep)
      outTrajs <- interpolateTrajs(taskTrajs, times)
      writeTrajs(outTrajs, filePath)
    },
    "velocity" = {
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
    stop("Unknown task class ", taskClass)
  )
}

collectRequiredTrajsFromTasks <- function(taskList) {
  lapply(taskList$list, \(task) {
    taskClass <- getClassAt(task, 2)
    switch(
      taskClass,
      "estiObsTrajs" = NULL,
      "newTrajs" = list(
        initialState = task$initialState,
        tMax = task$predictionTime[2]),
      "velocity" = NULL,
      stop("Unknown task class ", taskClass)
    )})
}

