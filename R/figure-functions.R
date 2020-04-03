# Functions for easier manipulation of figures

#' @importFrom colorspace coords
#' @importFrom scales percent
NULL

#' Percent labels on axes
#'
#' Converts a proportion to a percentage and adds "\%" behind them for use on a plot axis
#' @param x a vector of proportions
#' @param accuracy the interval size of calculation. Default = 1
#' @param decimal.mark What you would like to use as decimal mark, default = "."
#' @param suffix What to add as a suffix to all the labels, default is the correct English percentage symbol "\%".
#'               In Norwegian, percentages have a space between the number and the percentage, " \%".
#' @export
axis_percent = function(x, suffix="%", accuracy = 1, decimal.mark = ".", ...) {
  scales::percent(x, suffix=suffix, accuracy = accuracy, decimal.mark = decimal.mark, ...)
}

#' Darker-color Function
#'
#' Function for making a color x steps darker or lighter
#' Uses the CIELAB-colorspace for calculations
#' (instead of RGB).
#'
#' @param colors          a vector of colors.
#' @param level           how much darker to darken the color. negative values for making the color lighter
#'
#' @export
color_darker = function(colors, level = 5) {
  color_lab = as(colorspace::hex2RGB(colors), "LAB")
  color_lab@coords[, 1] = pmax(color_lab@coords[, 1] - level, 0)
  color_rgb = as(color_lab, "RGB")
  color_rgb@coords[] = pmax(color_rgb@coords, 0)
  color_rgb@coords[] = pmin(color_rgb@coords, 1)
  colorspace::hex(color_rgb)
}

#' Move-up Label Function
#'
#' Function that moves labels inside graphs if they collide.
#'
#' @param y        y-coordinate for the middle of text
#' @param text     the text inside the graph. Used to count how many lines is needed.
#' @param height   The height each line takes (in the coordiation system)
#' @export
move_up = function(y, text, height = .015) {
  text_new = text[order(y)]
  y = y[order(y)]
  lines = text_new %>% stringr::str_split("\n") %>% sapply(length)
  lower = y - lines * hoyde / 2
  upper = y + lines * hoyde / 2
  for (i in 2:length(y)) {
    avs = lower[i] - upper[i - 1]
    if (avs < 0) {
      y[i] = y[i] - avs
      upper[i] = upper[i] - avs
    }
  }
  y[match(text, text_new)]
}

#' Add n and percentages to labels
#'
#' The function adds the sample size and percentage behind each value in a factor,
#' purposefully for adding these to each label in a graph. E.g. "Male 5/10 (50%)"
#'
#' @param d dataframe with vector of labels. Sample size column must be named "n". Proportion column must be named "prop".
#'          Denominator must be in a column named "denominator".
#' @param x vector of labels
#' @param accuracy the interval-size for percent-calculations, defaults to 0.1 (1 decimal)
#' @param perc whether to add percentages (numerator/ denominator and percentage) or to just add
#'        the sample size for each label (n). Defaults to percentages.
#' @export
#' @examples
#' # data d_study is included in the ostrc-package
#' library(dplyr)
#' d_n_sports = d_study %>%
#'              count(sport) %>%
#'              mutate(denominator = sum(n),
#'                            prop = n/denominator) %>%
#'              arrange(desc(prop))
#'
#' d_n_sports = d_n_sports %>% mutate(labels = label_n(., sport)
#' d_n_sports
label_n = function(d, x, accuracy = 0.1, perc = TRUE){

  # column names in dataset used to test whether
  # required columns are missing later on
  cols = names(d)

  if(any(is.na(x))){
    x = x %>% replace_na("Missing (NA)")
    warning("Relabled NA values as Missing (NA).")
  }

  if(perc){
    # To paste percentages and n, the function requires the following columns in the provided dataset:
    required_cols = c("n", "denominator")

    # Stop function if numerator and denominator can't be found
    if(!all(required_cols %in% cols)) {
      missing_cols = which(!required_cols %in% cols)
      d_missing_cols = required_cols[missing_cols]
      stop(paste0("Dataset is missing required columns: ",stringr::str_c("'", d_missing_cols, "'", collapse=", "),"."))
    }

    numerator = d$n
    denominator = d$denominator
    prop = d$prop
    names_start = paste0(x, "\n", numerator)
    percents = axis_percent(prop, accuracy = accuracy)

    # If all the percentages have the same denominator, this can be
    # mentioned in the figure text or table rather than pasted for each subgroup
    if(n_distinct(denominator)==1){
      names = paste0(names_start," (",percents,")")
    }
    else{
      names = paste0(names_start, "/", denominator, " (",percents,")")
    }

  } else {

    if(!any("n" %in% cols)){
      stop(paste0("Dataset is missing required column: \"n\""))
    }

    names = paste0(x, "\n",d$n,"")
  }
  names
}

