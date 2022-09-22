writeTasks <- function(opts, writeOpts = TRUE) {
  opts <- asOpts(opts, c("Task", "List"))
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Tasks"))
  taskNr <- 0
  for (task in opts$list) {
    taskNr <- taskNr + 1
    writeOpts(task, file.path(opts$path, sprintf("task%02d", taskNr)))
  }
  return(taskNr)
}
