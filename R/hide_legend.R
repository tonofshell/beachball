#' Hide Legend
#'
#' Hides the legend of a ggplot visualization
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @export

hide_legend = theme(
  legend.position = "none",
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank()
)
