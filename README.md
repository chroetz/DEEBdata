# DEEBdata

This package allows to create a data base of simulated data for evaluating and comparing methods for estimation of differential equations.

Example:
```r
DEEBdata::runAll(
  dbPath = "NewDeebDb", # path to the database folder that should be created
  reps = 10, # number of repetions (each with new random seed) for creating a time series with specific settings
  optsPath = "path/to/optsDir", # path to a folder that contains Opts files describing the simulation to be executed.
# examples of such Opts files can be found in inst/RunOpts
  fromPackage = FALSE,
  truth = TRUE, obs = TRUE, task = TRUE, plot = FALSE,
  randomizeSeed = FALSE
)
```
