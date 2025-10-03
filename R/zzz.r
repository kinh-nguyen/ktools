.onLoad <- function(libname, pkgname) {
  okcol <<- c(
    base03 = "#002d38",
    base02 = "#093946",
    base01 = "#5b7279",
    base00 = "#657377",
    base0 = "#98a8a8",
    base1 = "#8faaab",
    base2 = "#f1e9d2",
    base3 = "#fbf7ef",
    yellow = "#ac8300",
    orange = "#d56500",
    red = "#f23749",
    magenta = "#dd459d",
    violet = "#7d80d1",
    blue = "#2b90d8",
    cyan = "#259d94",
    green = "#819500"
  )
  options(
    ggplot2.discrete.colour = ktools:::okabe,
    ggplot2.discrete.fill = ktools:::okabe
  )
  require("showtext")
  default_font <- "sans"
  require("ggplot2")
  if (curl::has_internet()) {
    default_font <- "Roboto Slab"
    font_add_google(default_font, default_font)
  }
  showtext_auto()
  k.theme <<- theme_light() +
    theme(
      text = element_text(family = default_font, colour = okcol["base00"]),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = okcol["base3"]),
      panel.border = element_rect(linewidth = 0.1, fill = NA, colour = "gray1"),
      strip.background = element_rect("white"),
      strip.text = element_text(color = "grey20", hjust = 0),
      legend.key = element_rect(fill = "transparent"),
      legend.background = element_rect(fill = "transparent"),
      legend.title = element_text(color = okcol["yellow"]),
      plot.caption = element_text(color = okcol["yellow"])
    )
  k.legend <<- function(x = .8, y = 1, dir = "horizontal", size = 0.25) {
    theme(
      legend.position = c(x, y),
      legend.direction = dir,
      legend.key.size = unit(size, "cm"),
      legend.title = element_text(size = 9),
      legend.box.background = elementalist::element_rect_round(fill = "white", color = "grey90", radius = unit(0.2, "snpc"))
    )
  }
  require("here")
  require("dplyr")
  require("tidyr")
  require("stringr")
  require("sf")
  sf::sf_use_s2(FALSE)
  invisible()
}
