plotTrajAndObs <- function(traj, obs, title, opts) {
  d <- ncol(traj) - 1
  if (d == 2) {
    projection2D <- getIdentityProjection()
  } else if (d > 2) {
    projection2D <- calculateProjection(traj[, -1], dim = 2)
  } else {
    stop("d invalid: ", d)
  }
  obs2D <- projection2D$project(as.matrix(obs[,-1]))
  truth2D <- projection2D$project(as.matrix(traj[,-1]))

  xlim <- if (length(opts$xlim) != 2) range(obs2D[,1], truth2D[,1]) else opts$xlim
  ylim <- if (length(opts$ylim) != 2) range(obs2D[,2], truth2D[,2]) else opts$ylim
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
