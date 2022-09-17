getParmsFunctionConst <- function(opts) {
  parmsFunction <- function(t, u, parms) {
    du <- array(opts$value, dim=length(u))
    return(du)
  }
  return(parmsFunction)
}


buildParmsSamplerConst <- function(opts) {
  parmsSampler <- function() list()
  return(parmsSampler)
}
