plotStateSpacePCA <- function(u) {
  pca <- stats::prcomp(u[,-1])
  projectionMatrix <- pca$rotation[,1:2]
  uPCA <- as.matrix(u[,-1]) %*% projectionMatrix
  plot(uPCA, type="l")
  graphics::grid()
  graphics::points(uPCA[1,,drop=FALSE], col=2, pch=2)
}
