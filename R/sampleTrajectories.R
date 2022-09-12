sampleConditional <- function(parmsSampler, fun, u0Sampler, opts) {

  success <- FALSE
  maxConditionTries <- if (length(opts$conditions) == 0) 1 else opts$maxConditionTries
  for (i in seq_len(maxConditionTries)) {
    parms <- parmsSampler()
    u0 <- u0Sampler()
    u <- solveOde(
      fun, u0,
      tMax = opts$tMax,
      tStep = opts$tStep,
      opts = opts$odeSolveropts,
      parms = parms)
    if (checkConditions(opts$conditions, u, fun, parms)) {
      success <- TRUE
      cat("o\n")
      break
    }
    cat("x")
  }
  if (!success) stop("Could not meet conditions.")
  message("Met condition after ", i, " tries.")

  return(list(u=u, parms = parms, u0 = u0))
}


sampleTrajectories <- function(opts) {

  opts <- readOpts(opts)

  fullPath <- file.path(opts$path, opts$name)
  if (!dir.exists(fullPath)) dir.create(fullPath)
  writeOpts(opts, file.path(fullPath, "_opts_truth"))

  parmsSampler <- buildParmsSampler(opts)
  fun <- getParmsFunction(opts)
  u0Sampler <- buildArraySampler(opts$u0Sampler, arrayDim = opts$d)

  set.seed(opts$seed)

  for (i in seq_len(opts$reps)) {
    message("Iteration ", i, " of ", opts$reps, ".")
    res <- sampleConditional(parmsSampler, fun, u0Sampler, opts)
    writeDeData(res$u, file.path(fullPath, sprintf("truth%04d.csv",i)))
    writeOpts(res$parms, file.path(fullPath, sprintf("truth%04d_meta",i)))
  }
}
