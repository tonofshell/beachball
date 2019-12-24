# color_pal

color_pal = function(n, type = "discrete", reverse = FALSE) {
  #Discrete - distinct categories
  #Segmented - two sided continuous with neutral center
  #Continuous - like segmented but w/o neutral center
  names_mid = c("grey", "pale yellow", "purple", "dark grey", "pine green")
  colors_mid = c("#C8DCC8", "#E6E673", "#A01EF0", "#5A5A5A", "#32AA41")

  order_main = c("mid offset", "light", "normal", "dark")
  colors_warm = c("#FFD747", "#FF7837", "#EB5028", "#D73219")
  colors_cool = c("#B1E651", "#1ED2D7", "#0FA0F5", "#0A55CD")

  if (n > 7) {
    final = switch (type,
                    "discrete" = colorRampPalette(c(colors_warm[1], colors_warm[2], colors_warm[3], colors_mid[3], colors_cool[3], colors_cool[1]))(n),
                    "segmented" = colorRampPalette(c(colors_warm[4], colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[2], colors_cool[3], colors_cool[4]))(n),
                    "continuous" = colorRampPalette(c(colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3]))(n),
                    "warm" = colorRampPalette(c(colors_mid[2], colors_warm, colors_mid[3]))(n),
                    "cool" = colorRampPalette(c(colors_mid[2], colors_cool, colors_mid[3]))(n)
    )
  } else {
    final = switch (as.character(n),
                    "1" = switch (type,
                                  "discrete" = colors_warm[3],
                                  "segmented" = colors_mid[4],
                                  "continuous" = colors_mid[3],
                                  "warm" = colors_warm[1],
                                  "cool" = colors_cool[3]
                    ),
                    "2" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3]),
                                  "segmented" = c(colors_warm[3], colors_cool[3]),
                                  "continuous" = c(colors_warm[3], colors_cool[3]),
                                  "warm" = c(colors_warm[2], colors_warm[3]),
                                  "cool" = c(colors_cool[2], colors_cool[3])
                    ),
                    "3" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1]),
                                  "segmented" = c(colors_warm[3], colors_mid[1], colors_cool[3]),
                                  "continuous" = c(colors_warm[3], colors_mid[3], colors_cool[3]),
                                  "warm" = c(colors_warm[1], colors_warm[2], colors_warm[3]),
                                  "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3])
                    ),
                    "4" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1], colors_mid[3]),
                                  "segmented" = c(colors_warm[4], colors_warm[3], colors_mid[1], colors_cool[4]),
                                  "continuous" = c(colors_warm[1], colors_warm[3], colors_mid[3], colors_cool[3]),
                                  "warm" = c(colors_warm[1], colors_warm[2], colors_warm[3], colors_warm[4]),
                                  "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4])
                    ),
                    "5" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1], colors_mid[3], colors_cool[1]),
                                  "segmented" = c(colors_warm[4], colors_warm[3], colors_mid[1], colors_cool[3], colors_cool[4]),
                                  "continuous" = c(colors_warm[1], colors_warm[3], colors_mid[3], colors_cool[3], colors_mid[5]),
                                  "warm" = c(colors_mid[2], colors_warm[1], colors_warm[2], colors_warm[3], colors_mid[3]),
                                  "cool" = c(colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3], colors_mid[3])
                    ),
                    "6" = switch (type,
                                  "discrete" = c(colors_warm[3], colors_cool[3], colors_warm[1], colors_mid[3], colors_cool[1], colors_warm[2]),
                                  "segmented" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[3], colors_cool[4]),
                                  "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_cool[1], colors_cool[2], colors_cool[3]),
                                  "warm" = colorRampPalette(c(colors_mid[2], colors_warm[-4], colors_mid[3]))(6),
                                  "cool" = colorRampPalette(c(colors_mid[2], colors_cool[-4], colors_mid[3]))(6)
                    ),
                    "7" = switch (type,
                                  "discrete" = colorRampPalette(c(colors_warm[1], colors_warm[2], colors_warm[3], colors_mid[3], colors_cool[3], colors_cool[1]))(7),
                                  "segmented" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                                  "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3]),
                                  "warm" = colorRampPalette(c(colors_mid[2], colors_warm[-4], colors_mid[3]))(7),
                                  "cool" = colorRampPalette(c(colors_mid[2], colors_cool[-4], colors_mid[3]))(7)
                    )
    )
  }

  if (reverse) {
    return(rev(final))
  }
  return(final)
}
