#' Beachball Map Theme
#'
#' A refined ggplot theme based on theme_light for creating maps/choropleths
#'
#' @param base_size The base size of elements
#' @param ... Other parameters to pass to theme_light
#' @param dark_theme Use theme_night for base theme
#' @return A ggplot theme
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 unit
#' @export

theme_map = function(..., base_size = 14, dark_theme = FALSE) {
  if (dark_theme) {
    bg_color = d_bg_color
    theme_night(base_size = base_size, ... = ...) +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_line(color = bg_color),
        panel.grid.minor = element_line(color = bg_color),
        panel.grid = element_line(color = bg_color),
        axis.text = element_text(color = bg_color),
        axis.text.x = element_text(color = bg_color),
        axis.text.y = element_text(color = bg_color),
        legend.position = "bottom",
        legend.key.width = unit(1, units = "cm")
      )
  } else {
    bg_color = 'white'
    theme_day(base_size = base_size, ... = ...) +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_line(color = bg_color),
        panel.grid.minor = element_line(color = bg_color),
        panel.grid = element_line(color = bg_color),
        axis.text = element_text(color = bg_color),
        axis.text.x = element_text(color = bg_color),
        axis.text.y = element_text(color = bg_color),
        legend.position = "bottom",
        legend.key.width = unit(1, units = "cm")
      )
  }

}
