getParmsFunctionConst <- function(opts) {
  parmsFunction <- function(t, u, parms) {
    du <- array(opts$value, dim=length(u))
    list(du)
  }
  return(parmsFunction)
}


buildParmsSamplerConst <- function(opts) {
  parmsSampler <- function() list()
  return(parmsSampler)
}
