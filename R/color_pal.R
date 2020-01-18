#' Beachball Color Palette
#'
#' Returns a color palette as a vector of RGB color hexcodes.
#'
#' @param n number of palette colors
#' @param type palette type
#' @param reverse reverses palette order
#' @param levels number of colors to return
#' @return A character vector of color hexcodes
#' @importFrom grDevices colorRampPalette
#' @export

color_pal = function(n, type = "discrete", reverse = FALSE, levels = n) {
  #Discrete - distinct categories
  #Segmented (Spectrum) - two sided continuous with neutral center
  #(Soft Spectrum) - two sided continuous with neutral center but only changes shade
  #(Monochrome)
  #Continuous - like segmented but w/o neutral center
  #Heatmap - continuous with changes from dark to light to accentuate "high" values
  names_mid = c("grey", "yellow", "purple", "dark_grey", "pine_green", "bright_yellow")
  colors_mid = c("#E6E6E6", "#E6D845", "#6D17E6", "#5A5A5A", "#2E9937", "#FFEA00")

  order_main = c("extra_light", "light", "normal", "dark", "alt_light")
  colors_warm = c("#E69A17", "#E65C17", "#E61717", "#D417E6", "#E68673")
  colors_cool = c("#5BCC28", "#17E6A1", "#17A1E6", "#5229CC", "#73E6E6")

  if (n > 7) {
    final = switch (type,
                    "discrete" = colorRampPalette(c(colors_warm[1], colors_warm[2], colors_warm[3], colors_mid[3], colors_cool[3], colors_cool[1]))(n),
                    "segmented" = colorRampPalette(c(colors_warm[4], colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[2], colors_cool[3], colors_cool[4]))(n),
                    "continuous" = colorRampPalette(c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4]))(n),
                    "warm" = colorRampPalette(c(colors_warm, colors_mid[3]))(n),
                    "cool" = colorRampPalette(c(colors_cool, colors_mid[3]))(n),
                    "heatmap" = colorRampPalette(c(colors_mid[5], colors_cool[3], colors_mid[3], colors_warm[3], colors_mid[6]))(n)
    )
  } else {
    final = switch (as.character(n),
                    "1" = switch (type,
                                  "discrete" = colors_warm[3],
                                  "segmented" = colors_mid[1],
                                  "continuous" = colors_mid[2],
                                  "warm" = colors_warm[1],
                                  "cool" = colors_cool[3],
                                  "heatmap" = colors_warm[4]
                    ),
                    "2" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3]),
                                  "segmented" = c(colors_warm[3], colors_cool[3]),
                                  "continuous" = c(colors_warm[3], colors_cool[3]),
                                  "warm" = c(colors_warm[2], colors_warm[3]),
                                  "cool" = c(colors_cool[2], colors_cool[3]),
                                  "heatmap" = c(colors_warm[4], colors_mid[6])
                    ),
                    "3" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1]),
                                  "segmented" = c(colors_warm[3], colors_mid[1], colors_cool[3]),
                                  "continuous" = c(colors_warm[3], colors_mid[2], colors_cool[3]),
                                  "warm" = c(colors_warm[1], colors_warm[2], colors_warm[3]),
                                  "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3]),
                                  "heatmap" = c(colors_warm[4], colors_warm[3], colors_mid[6])
                    ),
                    "4" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1], colors_mid[3]),
                                  "segmented" = colorRampPalette(c(colors_warm[3], colors_mid[1], colors_cool[3]))(4),
                                  "continuous" = c(colors_warm[3], colors_warm[2], colors_cool[2], colors_cool[3]),
                                  "warm" = c(colors_warm[1], colors_warm[2], colors_warm[3], colors_warm[4]),
                                  "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                                  "heatmap" = c(colors_cool[3], colors_warm[4], colors_warm[3], colors_mid[6])
                    ),
                    "5" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1], colors_mid[3], colors_cool[1]),
                                  "segmented" = c(colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[2], colors_cool[3]),
                                  "continuous" = c(colors_warm[3], colors_warm[2], colors_mid[2], colors_cool[2], colors_cool[3]),
                                  "warm" = colors_warm,
                                  "cool" = colors_cool,
                                  "heatmap" = c(colors_mid[5], colors_cool[3], colors_mid[3], colors_warm[3], colors_mid[6])
                    ),
                    "6" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1], colors_mid[3], colors_cool[1], colors_warm[2]),
                                  "segmented" = colorRampPalette(c(colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[2], colors_cool[3]))(6),
                                  "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_cool[1], colors_cool[2], colors_cool[3]),
                                  "warm" = c(colors_warm, colors_mid[3]),
                                  "cool" = c(colors_cool, colors_mid[3]),
                                  "heatmap" = colorRampPalette(c(colors_mid[5], colors_cool[3], colors_mid[3], colors_warm[3], colors_mid[6]))(6)
                    ),
                    "7" = switch (type,
                                  "discrete" = colorRampPalette(c(colors_warm[1], colors_warm[2], colors_warm[3], colors_mid[3], colors_cool[3], colors_cool[1]))(7),
                                  "segmented" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                                  "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3]),
                                  "warm" = colorRampPalette(c(colors_warm, colors_mid[3]))(7),
                                  "cool" = colorRampPalette(c(colors_cool, colors_mid[3]))(7),
                                  "heatmap" = colorRampPalette(c(colors_mid[5], colors_cool[3], colors_mid[3], colors_warm[3], colors_mid[6]))(7)
                    )
    )
  }

  if (reverse) {
    return(rev(final))
  }
  return(colorRampPalette(final)(levels))
}
