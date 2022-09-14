#' @importFrom rlang .data
plotVectorField <- function(trajs, fun, parms, title, axes) {
  d <- ncol(trajs$state)

  if (d == 2) {
    projection2D <- getIdentityProjection()
  } else if (d > 2) {
    projection2D <- calculateProjection(trajs$state, dim = 2)
  } else {
    stop("d invalid: ", d)
  }
  traj2D <- projection2D$project(trajs$state)

  nArrows <- 20
  rangeLenX <- diff(range(traj2D[,1]))
  rangeLenY <- diff(range(traj2D[,2]))
  maxLen <- pmax(rangeLenX, rangeLenY)
  s <- 0.1
  xGrid <- seq(
    mean(range(traj2D[,1])) - (0.5+s) * maxLen,
    mean(range(traj2D[,1])) + (0.5+s) * maxLen,
    length.out = nArrows)
  yGrid <- seq(
    mean(range(traj2D[,2])) - (0.5+s) * maxLen,
    mean(range(traj2D[,2])) + (0.5+s) * maxLen,
    length.out = nArrows)
  grid2D <- as.matrix(expand.grid(xGrid, yGrid))
  grid <- projection2D$embed(grid2D)
  field <-
    sapply(
      seq_len(nrow(grid)),
      \(i) unlist(fun(u = grid[i,], parms = parms))
    ) |>
    t()
  field2D <- projection2D$project(field)

  pltData <- tibble::tibble(
    x = grid2D[,1],
    y = grid2D[,2],
    vx = field2D[,1],
    vy = field2D[,2])
  trajData <- tibble::tibble(
    x = traj2D[,1],
    y = traj2D[,2],
    trajId = trajs$trajId)

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
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$trajId)) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL)
  if (!axes) {
    plt <-
      plt +
      ggplot2::theme_void()
  }
  plt <-
    plt +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = -2)) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::ggtitle(title)

  return(plt)
}
