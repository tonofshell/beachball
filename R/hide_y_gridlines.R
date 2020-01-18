#' Hide Y Gridlines
#'
#' Hides gridlines parallel to the y axis in a ggplot visualization
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @export

hide_y_gridlines = theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)
