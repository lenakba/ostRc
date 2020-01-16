# This script has a simple function for calculating Acute: Chronic Workload Ratios (ACWR).
# It is desgined based on the most common defintions of ACWR.
# The function is not designed to calculate any and every possible method
# for calculating ACWR.

#' @importFrom magrittr %>%
NULL

#--------------------------- Functions
#' Rolling Difference
#'
#' Function for calculating the difference between one value in a vector and the previous value.
#' The purpose is to calculate day-to-day or week-to-week differences in training load
#'
#' @param x a vector of training load values
#' @export
rolling_diff  = function(x){
  x_lag = dplyr::lag(x)
  x_diff = x-x_lag
  x_diff
}

#' Rolling Averages
#'
#' Function for calculating rolling averages on a chooseable number of days
#' Based on rollapplyr, not rollmean, as rollmean will only start calculating averages
#' at n values, while rollapplyr allows the user to decide preliminary values.
#'
#' @param x a vector of training load values. length(x) can be < n_days
#' @param n_days number of days the rolling average will calculate on (e.g. 7 for acute window)
#' @param window logical or numeric. If FALSE then FUN is only applied when
#'                   all indexes of the rolling window are within the observed time range.
#'                   If TRUE (default), then the subset of indexes that are in range are passed to FUN.
#'                   A numeric argument to partial can be used to determine the minimal window size
#'                  for partial computations. Defaults to TRUE and will calculate RA based on what you have available.
#' @export
ra = function(x, n_days, window = TRUE, ...){
  zoo::rollapplyr(x, n_days, mean, partial = window)
}

#' Exponential-Weighted Moving Averages
#'
#' Function for calculating exponentioally weighted moving averages (EWMA) on a choosable number of days
#' Warning from the TTR package: "Some indicators (e.g. EMA, DEMA, EVWMA, etc.)
#' are calculated using the indicators' own previous values, and are therefore
#' unstable in the short-term. As the indicator receives more data, its output becomes more stable."
#'
#' @param x a vector of training load values
#' @param n_days number of days the rolling average will calculate on (e.g. 7 for acute window)
#' @param window Number of periods to average over. Must be between 1 and nrow(x), inclusive.
#'               If set to 7, it will wait until 7 values before making a calculation
#' @export
ewma = function(x, n_days, window){
  TTR::EMA(x, n = window, ratio = 2/(1+n_days))
}

#' Acute: Chronic Workload Ratio
#'
#' Function for calculating the acute:chronic workload ratio.
#' It is desgined based on the most common defintions of ACWR.
#' The function is not designed to calculate any and every possible method for calculating ACWR.
#' Based off of Matt Sams R-guide for ACWR: https://mattsams89.netlify.com/post/2018-06-09-r-acwr-intro/r-acwr-intro/
#' This script uses tidyverse, but for data.table equivalents, see Matt Sams guide.
#' The user can choose between
#' Rolling Averages vs. Exponentially weighted moving averages
#' Coupled (default) vs. uncoupled calulation
#' number of days in acute period (default 7) and number of days in chronic period (default 28)
#' For EWMA, can decide whether the calculation should start from the first value, or
#' not until reaching value n
#'
#' @param x A vector of training load values.
#' @param n_acute Number of days included in the acute period (default = 7)
#' @param n_chronic Number of days included in the chronic period (default = 28)
#' @param window_acute Number of days to average over, for EWMA (default = 1). Must be between 1 and nrow(x), inclusive.
#'                  If set to 7, it will wait until 7 values before making a calculation
#' @param window_chronic Number of days to average over, for EWMA (default = 1). Must be between 1 and nrow(x), inclusive.
#'                  If set to 28, it will wait until 28 values before making a calculation
#' @param fun Which aggregation function to use, choose between "ra" for rolling averages
#'                  and "ewma" for exponentially weighted moving averages. Defaults to "ra".
#'                  If the vector of training loads contains any missing values (NA),
#'                  "ra" will be based on values before, and then subsequently values after the missing values,
#'                  but will never intermix values before and after missing values in the calculation.
#'                  "ewma" handles leading missing values, but any vectors with non-monotonous NA or ending in NA will result in error.
#' @param coupling Whether the acute training loads is to be included in the denominator, "coupled" (default)
#'                  or not, "uncoupled". Based on equation in https://bjsm.bmj.com/content/53/16/988 .
#'                  Assumes uncoupled is the same as the chronic period without the 7 days acute (21 days instead of 28), not
#'                  that it skips the first 7 days before including 4 weeks of data (28 days).
#'                  Equation is only equivalent when n has reached the full period (e.g. all 28 values are available).
#' @export
#' @examples
#' tl = sample(500:1300, 28, replace = TRUE)
#' acwr(tl)
#' acwr(window_acute = 7, window_chronic = 21, fun = "ewma", coupling = "uncoupled")
#'
#' acwr(d_basketball$tl)
#' acwr(d_basketball$tl, fun = "ewma")
#' acwr(d_basketball$tl, fun = "ewma", coupling = "uncoupled")
acwr = function(x, n_acute = 7, n_chronic = 28, window_acute = 1, window_chronic = 1, fun = "ra", coupling = "coupled"){

  stopifnot(is.numeric(x))

  # warning at vectors shorter than the chosen acute length
  if(length(x) < n_acute){
    length = length(x)
    warning(paste0("Object length is less than acute period. Length = ",length,", Acute = ",n_acute,""))
  }

  # Rolling Averages (RA)
  if(fun == "ra"){
    # warning at missing values
    if(any(is.na(x))){
      n_na = sum(is.na(x))
      warning(paste0("Ignoring ",n_na," missing values."))
    }
    acute = ra(x, n_acute, window_acute)
    chronic = ra(x, n_chronic, window_chronic)
  }

  # Exponentially-weighted moving averages(EWMA)
  if(fun == "ewma"){
    # finding first non-missing value
    first_nonmissing = min(which(!is.na(x)))
    # see if there are any missing values from that point on
    if(any(is.na(x[first_nonmissing:length(x)]))){
    stop("Error: EWMA cannot be calculated on vectors with non-leading NA elements.")
    }
    acute = ewma(x, n_acute, window_acute)
    chronic = ewma(x, n_chronic, window_chronic)
  }

  # calculating acwr base values
  acwr_base = acute/chronic

  # shift the ACWR values down one day to accurately reflect the effect of the previous day's training
  acwr_lag = dplyr::lag(acwr_base)

  # Optionally, uncoupled ACWR ratios
  if(coupling == "uncoupled"){
    acwr = (3 * acwr_lag)/(4 - acwr_lag)
  } else if (coupling == "coupled") {
    acwr = acwr_lag
  }

  # return ACWR-values
  acwr
}

# ###------------------------------------------------------- Quality Control
#
# # testing the function on real data inlcuded in the OSTRC package
# # downloaded from https://mattsams89.netlify.com/post/2018-06-09-r-acwr-intro/r-acwr-intro/tl-data.csv
#
# # Adding columns of EWMA-ACWR and RA-ACWR calculated per athlete
# d_basketball = d_basketball %>% dplyr::group_by(athlete) %>% dplyr::mutate(acwr_ra = acwr(tl),
#                                        acwr_ewma = acwr(tl, fun = "ewma"),
#                                        acwr_ewma_uc = acwr(tl, fun = "ewma", coupling = "uncoupled"))
#
# # Since Matt Sams has done ACWR calculations on the exampled dataset previously,
# # we can check that our results are the same.
# # Also, the figure code below should create figures exactly equal to the figures in his blogpost:
# # https://mattsams89.netlify.com/post/2018-06-09-r-acwr-intro/r-acwr-intro/
#
# # Figures are made with the exact same code as Matt sams for reproducability.
# # Figure
# ggplot(d_basketball, aes(x = training.date)) + geom_col(aes(y = tl)) +
#   geom_line(aes(y = acwr_ra * 1000)) +
#   scale_y_continuous(sec.axis = sec_axis(~./1000, name = 'RA ACWR')) +
#   theme_bw() + facet_wrap(~athlete) +
#   labs(title = 'TL with RA ACWR', x = 'Training Date', y = 'TL')
#
# # Figure 3
# ggplot(d_basketball, aes(x = training.date)) + geom_col(aes(y = tl)) +
#   geom_line(aes(y = acwr_ewma * 1000)) +
#   scale_y_continuous(sec.axis = sec_axis(~./1000, name = 'EWMA ACWR')) +
#   theme_bw() + facet_wrap(~athlete) +
#   labs(title = 'TL with EWMA ACWR', x = 'Training Date', y = 'TL')
#
# # Corresponds to the coupled vs. uncoupled figure (Figure 5)
# ggplot(d_basketball %>% filter(athlete == 'Urja Chaudhry'), aes(x = as.Date(training.date), group = 1)) +
#   geom_col(aes(y = tl)) + geom_line(aes(y = acwr_ewma * 1000, colour = 'Coupled')) +
#   geom_line(aes(y = acwr_ewma_uc * 1000, colour = 'Uncoupled')) +
#   scale_y_continuous(sec.axis = sec_axis(~./1000, name = 'ACWR')) +
#   labs(title = 'Coupled vs. Uncoupled', x = 'Training Date', y = 'TL', colour = 'Method') +
#   theme_bw() + theme(legend.position = 'bottom')

