.onLoad <- function(libname, pkgname) {
  ConfigOpts::addPackageToPathDefaults(
    system.file("defaultOpts", package=pkgname, lib.loc=libname))
  ConfigOpts::addPackageToPathDefaults(
    system.file("defaultOpts", package="DEEBeval", lib.loc=libname))
  invisible(NULL)
}
