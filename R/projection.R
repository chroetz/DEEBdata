getIdentityProjection <- function() {
  list(
    project = \(x) x,
    embed = \(x) x)
}

calculateProjection <- function(y, dim) {
  center <- colMeans(y)
  yCentered <- sweep(y, 2, center)
  pca <- stats::prcomp(
    yCentered,
    retx = FALSE,
    center = FALSE,
    scale. = FALSE)
  projectionMatrix <- pca$rotation[, seq_len(dim), drop=FALSE]
  centerCutOff <- center %*% pca$rotation[, -seq_len(dim), drop=FALSE]
  list(
    project = \(x) x %*% projectionMatrix,
    embed = \(x) cbind(x, matrix(rep(centerCutOff, each = nrow(x)), nrow = nrow(x))) %*% t(pca$rotation))
}

