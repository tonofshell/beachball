#' Hide X Gridlines
#'
#' Hides gridlines parallel to the x axis in a ggplot visualization
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @export

hide_x_gridlines = theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)
