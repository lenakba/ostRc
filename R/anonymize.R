
#' The function make_ano_func() needs two arguments.
#' The first (x) is a vector with IDs and the other (startnr)
#' is the starting number for the anonymized IDs (the standard value is 101).
#' The function provides a anonymization function which will make sure that IDs that exist in more than one dataset
#' will get the same IDs in each dataset its used on.
#' developed by Stian Tufte Veisene and Karl Ove Hufthammer
#'
#' @param x Vector with IDs to anonymize in the future
#' @param startnr The Starting number for the anonymized ID. Defaults to 101.
#' @export
make_anonymize_func = function(x, startnr = 101) {
  if (any(is.na(x))) {
    warning("ID-vector contains NA-values")
  }
  from = sort(unique(x))
  to = as.integer(sample.int(length(from)) + startnr - 1)

  anonymize_func = function(x_sample) {
    if (any(is.na(x_sample))) {
      warning("ID-vector contains NA-values")
    }
    if (!is_empty(na.omit(setdiff(x_sample, x)))) {
      warning ("The ID-vector contains new (unknown) IDs")
    }
    to[match(x_sample, from)]
  }
  anonymize_func
}


#' The helper function anonymize() is time-saving in one-dataset cases.
#' With this function, there is no need to call the anonymization function twice on the same data.
#'
#' @param x Vector with IDs to anonymize in the future
#' @param startnr The Starting number for the anonymized ID. Defaults to 101.
#' @export
anonymize = function(x, startnr = 101) {
  make_ano_func(x, startnr = startnr)(x)
}
