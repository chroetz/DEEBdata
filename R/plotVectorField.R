#' @importFrom rlang .data
plotVectorField <- function(traj, fun, parms) {
  d <- ncol(traj) - 1

  if (d == 2) {
    projectionMatrix <- matrix(c(1,0,0,1), nrow=2)
  } else if (d > 2) {
    pca <- stats::prcomp(traj[,-1])
    projectionMatrix <- pca$rotation[,1:2]
  } else {
    stop("d invalid")
  }
  traj2D <- as.matrix(traj[,-1]) %*% projectionMatrix

  nArrows <- 20
  rangeLenX <- diff(range(traj2D[,1]))
  rangeLenY <- diff(range(traj2D[,2]))
  s <- 0.1
  xGrid <- seq(
    min(traj2D[,1])-s*rangeLenX,
    max(traj2D[,1])+s*rangeLenX,
    len=nArrows)
  yGrid <- seq(
    min(traj2D[,2])-s*rangeLenY,
    max(traj2D[,2])+s*rangeLenY,
    len=nArrows)
  gridPC <- as.matrix(expand.grid(xGrid, yGrid))
  grid <- gridPC %*% t(projectionMatrix)
  field <- sapply(
    seq_len(nrow(grid)),
    \(i) unlist(fun(u = grid[i,], parms = parms)))
  fieldPC <- t(field) %*% projectionMatrix

  pltData <- tibble::tibble(
    x = gridPC[,1],
    y = gridPC[,2],
    vx = fieldPC[,1],
    vy = fieldPC[,2])
  trajData <- tibble::tibble(
    x = traj2D[,1],
    y = traj2D[,2])

  pltData <-
    pltData |>
    dplyr::mutate(speed = sqrt(.data$vx^2 + .data$vy^2))
  maxSpeed <- max(pltData$speed)

  sizeX <- max(diff(range(xGrid))) / nArrows
  sizeY <- max(diff(range(yGrid))) / nArrows
  size <- mean(c(sizeX, sizeY)) * sqrt(2)
  plt <- ggplot2::ggplot(pltData) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        color = .data$speed,
        xend = .data$x+.data$vx/maxSpeed*size,
        yend = .data$y+.data$vy/maxSpeed*size),
      arrow = ggplot2::arrow(length = grid::unit(0.1, "cm")),
      size = 0.6) +
    ggplot2::scale_colour_continuous(
      low = "grey80",
      high = "darkred") +
    ggplot2::geom_path(
      data = trajData,
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      size = 1) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed(ratio = 1)

  return(plt)
}
