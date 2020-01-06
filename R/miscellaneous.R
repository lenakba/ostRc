
#' Smart Rounding Function
#'
#' Considers the smallest difference between values of a vector when rounding.
#' Use with caution. Will not work on vectors where differences are smaller than 0.1
#'
#' @param x          a numeric vector with decimals to be rounded
#'
#' @export
smart_round = function(x){

  if(length(x)<=1){
    stop("x is not a vector of length >= 1")
  }

  diff = diff(na.omit(x))

  if(length(diff)<=1){
    largest_diff = abs(diff)
  }else{
    largest_diff = abs(max(diff)-min(diff))
  }

  if(largest_diff >= 2){
    x = round(x)
  } else if(largest_diff < 2 & largest_diff > 0.1){
    x = round(x, 1)
  } else if(largest_diff <= 0.1){
    x = round(x, 2)
  }
  x
}
