# TODO: move to own plots package

plotTimeDep <- function(truth, obs, title="") {
  if (is.null(obs)) {
    obs <- truth[0,]
  }

  data <-
    dplyr::bind_rows(
      truth |> dplyr::mutate(kind = "truth")) |>
    unpackStateLong()

  obs <-
    obs |>
    dplyr::mutate(kind = "obs") |>
    unpackStateLong()

  plt <-
    ggplot2::ggplot(data, ggplot2::aes(
      x = .data$time,
      y = .data$state,
      color = .data$kind,
      group = paste0(.data$trajId, .data$kind)
    )) +
    ggplot2::geom_path() +
    ggplot2::geom_point(
      data = obs,
      mapping = ggplot2::aes(color = NULL, group = NULL),
      size = 0.1,
      alpha = 0.5
    ) +
    ggplot2::facet_wrap(ggplot2::vars(dim), ncol = 1, scales = "free_y") +
    ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)

  return(plt)
}

unpackStateLong <- function(trajs) {
  trajs |>
    dplyr::mutate(tmp = tibble::as_tibble(structure(.data$state, dimnames = list(
      NULL, paste0("state", 1:ncol(.data$state))
    )))) |>
    tidyr::unpack(.data$tmp) |>
    dplyr::select(-.data$state) |>
    tidyr::pivot_longer(
      dplyr::starts_with("state"),
      names_to = "dim",
      values_to = "state",
      names_transform =  ~ stringr::str_sub(., start = 6)
    ) |>
    dplyr::mutate(dim = as.integer(.data$dim))
}

