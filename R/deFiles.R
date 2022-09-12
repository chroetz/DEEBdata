writeDeData <- function(u, file) {
  uFormated <- cbind(
    time = format(u[, 1], digits=3),
    as.data.frame(format(u[, -1], digits=1, scientific=FALSE, nsmall=8)))
  utils::write.csv(
    uFormated,
    file = file,
    quote = FALSE,
    row.names = FALSE)
}


readDeData <- function(file) {
  utils::read.csv(file)
}
