writeDeData <- function(trajs, file) {
  colnames(trajs$state) <- paste0("state", seq_len(ncol(trajs$state)))
  trajsFormated <- cbind(
    trajId = if ("trajId" %in% names(trajs)) format(trajs$trajId) else "0",
    time = format(trajs$time),
    as.data.frame(format(trajs$state, digits = 1, scientific=FALSE, nsmall=8)))
  utils::write.csv(
    trajsFormated,
    file = file,
    quote = FALSE,
    row.names = FALSE)
}


readDeData <- function(file) {
  df <- utils::read.csv(file)
  dataFrame2TrajsTibble(df)
}

dataFrame2TrajsTibble <- function(df) {
  tibble::tibble(
    time = df$time,
    trajId = df$trajId,
    state = as.matrix(df[setdiff(names(df), c("time", "trajId"))]))
}

matrix2TrajsTibble <- function(mat) {
  tibble::tibble(
    time = mat[,"time"],
    trajId = mat[,"trajId"],
    state = mat[, setdiff(colnames(mat), c("time", "trajId"))])
}
