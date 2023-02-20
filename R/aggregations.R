# This script has a simple function for calculating Acute: Chronic Workload Ratios (ACWR).
# It is desgined based on the most common defintions of ACWR.
# The function is not designed to calculate any and every possible method
# for calculating ACWR.

#' @importFrom magrittr %>%
#' @importFrom zoo rollapplyr
#' @importFrom TTR EMA
#' @importFrom dplyr lag
NULL

#--------------------------- Functions
#' Rolling Difference
#'
#' Function for calculating the difference between one value in a vector and the previous value.
#' The purpose is to calculate day-to-day or week-to-week differences in training load
#'
#' @param x a vector of training load values
#' @param abs Whether to calculate the absolute values of the differences TRUE, or not, FALSE. Defaults to FALSE.
#' @export
moving_range  = function(x, abs = FALSE){
  x_lag = dplyr::lag(x)
  x_diff = x-x_lag
   if(abs){
   x_diff = abs(x_diff)
   }
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
#' Function for calculating exponentially weighted moving averages (EWMA)
#' on a choosable number of days or weeks.
#' Warning from the TTR package: "Some indicators (e.g. EMA)
#' are calculated using the indicators' own previous values, and are therefore
#' unstable in the short-term. As the indicator receives more data, its output becomes more stable."
#'
#' @param x a vector of values that you wish to calculate EWMA on. For instance, training load values.
#' @param n numeric. The size of the window for calculating EWMA. I.e. 28 for 28 days, or 4 for 4 weeks, depending on the chosen time unit.
#' @param wilder logical; wilder=FALSE (the default) uses an exponential smoothing ratio of 2/(n+1),
#' same as in williams et al. 2016. wilder=TRUE uses Welles Wilder's exponential smoothing ratio of 1/n
#' @export
ewma = function(x, n, wilder = FALSE){
  TTR::EMA(x, n = n, wilder)
}
