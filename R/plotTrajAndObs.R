plotTrajAndObs <- function(truth, obs, title, opts) {
  d <- ncol(truth) - 1
  if (d == 1) {
    obs2D <- as.matrix(obs)
    truth2D <- as.matrix(truth)
  } else if (d == 2) {
    obs2D <- as.matrix(obs[,-1])
    truth2D <- as.matrix(truth[,-1])
  } else if (d >= 3) {
    pca <- stats::prcomp(truth[,-1])
    projectionMatrix <- pca$rotation[,1:2]
    obs2D <- as.matrix(obs[,-1]) %*% projectionMatrix
    truth2D <- as.matrix(truth[,-1]) %*% projectionMatrix
  } else {
    stop("d is ", d)
  }
  xlim <- if (is.null(opts$xlim)) range(obs2D[,1], truth2D[,1]) else opts$xlim
  ylim <- if (is.null(opts$ylim)) range(obs2D[,2], truth2D[,2]) else opts$ylim
  plot(NA, xlim=xlim, ylim=ylim, axes = opts$axes, asp=1)
  graphics::grid()
  if (opts$border == "circle") {
    angleSeq <- seq(0, 2*pi, len=200)
    graphics::lines(
      mean(xlim) + cos(angleSeq)*diff(xlim)/2,
      mean(ylim) + sin(angleSeq)*diff(ylim)/2,
      col = "darkgray")
  } else if (opts$border == "rect") {
    graphics::box(col = "darkgray")
  } else {
    stop("Unrecognized name ", opts$border)
  }
  graphics::points(obs2D, col=3, pch=20)
  graphics::lines(truth2D)
  graphics::points(truth2D[1,,drop=FALSE], col=2, pch=3, lwd=2)
  graphics::text(
    x = mean(xlim),
    y = pmax(mean(ylim)+0.47*diff(xlim), mean(ylim)+0.47*diff(ylim)), # asp=1
    label = title)
}
