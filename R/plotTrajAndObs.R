plotTrajAndObs <- function(trajs, obs, title="", opts=makeOpts("PlotOpts"), esti=NULL) {
  d <- getDim(trajs)
  if (d == 2) {
    projection2D <- getIdentityProjection()
  } else if (d > 2) {
    projection2D <- calculateProjection(trajs$state, dim = 2)
  } else {
    stop("d invalid: ", d)
  }
  obs2D <- projection2D$project(obs$state)
  truth2D <- projection2D$project(trajs$state)

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
  for (id in unique(trajs$trajId)) {
    traj <- truth2D[trajs$trajId==id, ]
    graphics::lines(traj)
    graphics::points(traj[1,,drop=FALSE], col=2, pch=3, lwd=2)
  }
  if (!is.null(esti)) {
    esti2D <- projection2D$project(esti$state)
    for (id in unique(esti$trajId)) {
      traj <- esti2D[esti$trajId==id, ]
      graphics::lines(traj, col=4)
      graphics::points(traj[1,,drop=FALSE], col=4, pch=3, lwd=2)
    }
  }
  graphics::text(
    x = mean(xlim),
    y = pmax(mean(ylim)+0.47*diff(xlim), mean(ylim)+0.47*diff(ylim)), # asp=1
    label = title)
}
