sampleConditional <- function(parmsSampler, f, u0Sampler, opts) {

  success <- FALSE
  for (i in seq_len(opts$maxConditionTries)) {
    parms <- parmsSampler()
    u0 <- u0Sampler()
    u <- solveOde(
      f, u0,
      tMax = opts$tMax,
      tStep = opts$tStep,
      opts = opts$odeSolveropts,
      parms = parms)
    if (checkConditions(opts$conditions, u, f, parms)) {
      success <- TRUE
      break
    }
  }
  if (!success) stop("Could not meet condition.")
  message("Met condition after ", i, " tries.")

  return(list(u=u, parms = parms, u0 = u0))
}


sampleTrajectories <- function(opts) {

  opts <- readOpts(opts)

  fullPath <- file.path(opts$path, opts$name)
  if (!dir.exists(fullPath)) dir.create(fullPath)
  writeOpts(opts, file.path(fullPath, "_opts_truth"))

  parmsSampler <- buildParmsSampler(opts)
  f <- getParmsFunction(opts)
  u0Sampler <- buildSingleSampler(opts$u0Sampler)

  set.seed(opts$seed)

  for (i in seq_len(opts$reps)) {
    message("Iteration ", i, " of ", opts$reps, ".")
    res <- sampleConditional(parmsSampler, f, u0Sampler, opts)
    writeDeData(res$u, file.path(fullPath, sprintf("truth%04d.csv",i)))
    writeOpts(res$parms, file.path(fullPath, sprintf("truth%04d_meta",i)))
  }
}
