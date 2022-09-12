getParmsFunction <- function(opts) {
  if (opts$className == "localConst") {
    parmsFunction <- getParmsFunctionLocalConst(opts)
  } else {
    stop("Unrecognized name ", opts$className)
  }
  return(parmsFunction)
}


buildParmsSampler <- function(opts) {
  if (opts$className == "localConst") {
    parmsSampler <- buildParmsSamplerLocalConst(opts)
  } else {
    stop("Unrecognized name ", opts$className)
  }
  return(parmsSampler)
}
