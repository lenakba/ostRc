#' Basketball data
#'
#' Undocumented data on the training load of basketball players during a 2017 Fall season.
#' Available from this webpage: https://mattsams89.netlify.com/post/2018-06-09-r-acwr-intro/r-acwr-intro/
#'
#' @docType data
#'
#' @usage data(d_basketball)
#'
#' @format A data frame with 666 rows and 4 variables:
#' \describe{
#'   \item{Season}{The season in which the basketballplayers played}
#'   \item{training.date}{The date of training}
#'   \item{athlete}{Which athlete the data belongs to}
#'   \item{tl}{The training load}
#' }
#'
#' @source \url{https://mattsams89.netlify.com/post/2018-06-09-r-acwr-intro/r-acwr-intro/tl-data.csv}
"d_basketball"


#' TL->Injury data
#'
#' Data on a review of clinical studies on the relationship between training load or workload and injury.
#'
#' @docType data
#'
#' @usage data(d_study)
#'
#' @format A data frame with 83 rows and 53 variables
#'
#' @source \url{https://mattsams89.netlify.com/post/2018-06-09-r-acwr-intro/r-acwr-intro/tl-data.csv}
"d_study"
