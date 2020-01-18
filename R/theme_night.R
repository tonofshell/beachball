#' Beachball Dark Theme
#'
#' A refined ggplot theme based on theme_light with a dark background
#'
#' @param base_size The base size of elements
#' @param ... Other parameters to pass to theme_light
#' @return A ggplot theme
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @export

theme_night = function (..., base_size = 14) {
  half_line = base_size/2
  theme_light(base_size = base_size, ...) +
    theme(
      plot.background = element_rect(color = d_bg_color, fill = d_bg_color),
      panel.background = element_rect(color = d_bg_color, fill = d_bg_color),
      plot.title = element_text(face = "bold", color = "white"),
      plot.subtitle = element_text(size = base_size - 2, face = "italic", color = "white"),
      plot.caption = element_text(color = "grey50"),
      axis.title = element_text(face = "bold", color = "white"),
      legend.title = element_text(color = "white"),
      legend.background = element_rect(fill = d_bg_color),
      legend.key = element_rect(fill = d_bg_color),
      legend.text = element_text(color = "grey75"),
      axis.text.x = element_text(color = "grey50"),
      axis.text.y = element_text(color = "grey50"),
      panel.grid.major = element_line(colour = "grey50"),
      panel.grid.minor = element_line(colour = "grey25")
    )
}
