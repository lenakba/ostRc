#' Confidence interval for binomial distribution
#'
#' Use the Wilson-interval, as recommended in «Binomial confidence intervals
#' and contingency tests: mathematical fundamentals and the evaluation of
#' alternative methods», by Sean Wallis, University College London.
#'
#' Returns a tibble with lower and upper limits for a 95 \%
#' wilson-confidence interval.
#' @param x Number of successes/events in the attempt.
#' @param n Number of independent attempts.
#' @export
ci_bin = function(x, n) {
  ci = binom::binom.wilson(x, n)
  tibble(lower = pmax(0, ci$lower), # A fix as limist can to a tiny bit outside [0,1]
         upper = pmin(1, ci$upper))
}
