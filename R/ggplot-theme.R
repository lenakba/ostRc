# A script for pretty figures and graphs.
# Includes theme functions and color definitions for use in ggplot2.

#' @import ggplot2
NULL

##---------------Functions---------------------
######
#' Theme Base
#'
#' Function for making a theme for ggplot-plots
#' Based on standard parameters.
#'
#' @param text_size The fontsize of text in the figure
#' @param panel_spacing The amount of "air" between figures in a panel.
#'                           Is given in points, "pt", defaults to "13".
#'                           Little spacing may make a cluttered look, but might be necessary for
#'                           some figures to fit.
#' @export
theme_base = function(text_size = 12, panel_spacing = "13"){

  theme = theme_light(base_size=text_size)

  # Panel header with a warmer color
  theme$strip.background$fill="#f3f1ee"
  theme$strip.background$colour="#e4e0da"
  theme$strip.text.x = element_text(colour="black")

  # Background grid with same colors as panel header.
  # Note: panel.background is drawn *under* axis lines
  #       and graphical elements, while panel.border is drawn *on top*.
  #       Therefore, we remove panel.border.
  theme$panel.border = element_blank()
  theme$panel.background$colour=theme$strip.background$colour
  theme$panel.grid.major$colour=theme$strip.background$colour
  theme$panel.grid.minor$colour=theme$strip.background$fill

  # Allow optional amount of spacing between figures in a panel
  theme$panel.spacing=unit(panel_spacing ,"pt")

  # Horizontal y-axis label.
  # https://policyviz.com/2016/09/07/where-to-position-the-y-axis-label/
  theme$axis.title.y$angle=0

  # Remove air to the left of y-axis label and add air to its right.
  # Remove air under the x-axis label, and add air over it.
  # Remove almost all excess air around the figure.
  #
  # (Note: Margins are 3 points to ensure no letters
  # are cut off)
  theme$axis.title.y$margin=margin(r=theme$text$size/2)
  theme$axis.title.x$margin=margin(t=theme$text$size/2)
  theme$plot.margin=margin(3, 3, 3, 3)

  theme

}

#' Remove x gridlines from ggplot
#' @export
remove_x = theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank())

#' Remove y gridlines from ggplot
#' @export
remove_y = theme(panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank())

#' Remove x tick marks for categories
#' (equivalent to «major breaks» on the axis).
#' Useful for bargraphs with categorical values, as
#' the ticmarks are unecessary
#' @export
remove_x_ticks = theme(axis.ticks.x = element_blank())

#' Remove y tick marks for categories
#' (equivalent to «major breaks» on the axis).
#' Useful for bargraphs with categorical values, as
#' the ticmarks are unecessary
#' @export
remove_y_ticks = theme(axis.ticks.y = element_blank())

#' Theme Bar
#'
#' Function for theme relevant for horizontal barcharts
#'
#' @export
theme_barh = function(text_size = 12, ...){
  theme = theme_base(text_size = text_size)
  # removes y-axis gridlines
  theme$panel.grid.major.y = element_blank()
  theme$panel.grid.minor.y = element_blank()
  theme$axis.ticks.y = element_blank()
  theme
}

#' Theme Dotplot
#'
#' Function for theme relevant for Cleveland dotcharts
#'
#' @export
theme_dot = function(text_size = 12, ...){
  theme = theme_base(text_size = text_size)
  # removes x-axis gridlines
  theme$panel.grid.major.x = element_blank()
  theme$panel.grid.minor.x = element_blank()
  theme$axis.ticks.y = element_blank()
  theme
}

#' Theme Line
#'
#' function for theme relevant for linecharts
#'
#' @export
theme_line = function(text_size = 12, ...){
  theme = theme_base(text_size = text_size)
  theme$panel.grid.major.x = element_blank()
  theme$panel.grid.minor.x = element_blank()
  theme
}

#' Bars should start adjecent to the axis, but have some air next to it.
#' which means assymetric expand-values.
#' this is predefined in the object for expand-arguments in ggplot
#' @export
expand_bar = expand_scale(mult = c(0.0, .05), add = 0)


# NIH colors
# based on powerpoint styleguide
color = function(...) rgb(..., max = 255)
nih_darkblue = color(0, 17, 55)
nih_darkblue2 = color(0, 13, 45)
nih_red = color(153, 0, 0)
nih_lightgrey = color(221, 221, 221)
nih_yellow = color(255, 153, 0)
nih_lightyellow = color(255, 255, 153)
nih_green = color(0, 128, 0)
nih_darkgreen = color(0, 115, 0)
nih_nude = color(255, 202, 170)
nih_red_contrast = color(220, 0, 0)
nih_blue_contrast = color(0, 56, 180)
nih_white = color(255, 255, 255)

#' NIH colors based on powerpoint stylguide.
#' 6 distinct colors
#' @export
nih_distinct = c(nih_yellow, nih_red, nih_green, nih_darkblue, nih_nude, nih_lightyellow)

#' NIH colors based on powerpoint stylguide.
#' 2 contrast colors
#' @export
nih_contrast = c(nih_red_contrast, nih_blue_contrast)

# using red to make darker en lighter hue
nih_darkred = color_darker(nih_red, 10)
nih_lightred = color_darker(nih_red, -10)

#' Ordinal NIH colorscale based on red
#' @export
nih_ordinal = c(nih_lightred, nih_red, nih_darkred)

#' British Journal of Sports Medicine (BJSM) green
#' @export
bjsm_green = "#346702"

#' British Journal of Sports Medicine (BJSM) blue
#' @export
bjsm_blue = "#0070b9"
