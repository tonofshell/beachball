#' Beachball Light Theme
#'
#' A refined ggplot theme based on theme_light
#'
#' @param base_size The base size of elements
#' @param ... Other parameters to pass to theme_light
#' @return A ggplot theme
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @export

theme_day = function (..., base_size = 14) {
  half_line = base_size/2
  theme_light(base_size = base_size, ...) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = base_size - 2, face = "italic"),
      plot.caption = element_text(color = "grey50"),
      axis.title = element_text(face = "bold"),
      legend.text = element_text(color = "grey25"),
      axis.text.x = element_text(color = "grey50"),
      axis.text.y = element_text(color = "grey50"),
      # strip.text.x = element_text(size = base_size / 20),
      # strip.text.y = element_text(size = base_size / 20),
      panel.grid.major = element_line(colour = "grey50"),
      panel.grid.minor = element_line(colour = "grey75")
    )
}
