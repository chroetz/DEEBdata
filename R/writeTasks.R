writeTasks <- function(opts, writeOpts = TRUE) {
  message("Creating Tasks")
  opts <- asOpts(opts, c("Task", "List"))
  if (!dir.exists(opts$path)) dir.create(opts$path)
  if (writeOpts) writeOpts(opts, file.path(opts$path, "Opts_Tasks"))
  taskNr <- 0
  for (task in opts$list) {
    taskNr <- taskNr + 1
    writeOpts(task, file.path(opts$path, DEEBpath::taskFile(taskNr = taskNr)))
  }
  return(taskNr)
}
