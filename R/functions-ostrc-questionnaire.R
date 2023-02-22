#' @importFrom magrittr %>%
#' @import tibble
#' @import dplyr

# Only responses to the first OSTRC question
# (“Have you had any difficulties participating in
# normal training and competition due to injury,
# illness or other health problems during the past week?”)
# that were not “Full participation without health problems”
# were considered to be indicative of a health problem.

#' Function to find substantial injuries given the definition
#' in the original OSTRC-O paper: doi.org/10.1136/bjsports-2012-091524.
#' Returns a vector with 0 for non-substantial, 1 for substantial.
#' Substantial health problems are defined as injury or illness
#' that reduced training volumes or performance to a moderate extent or worse,
#' or to a complete absence from sports.
#' The function follows the original recipe of extracting substantial injuries
#' from the OSTRC questionnaire:
#' any reply of “To a moderate extent”, “To a major extent” or “Cannot participate at all”,
#' on EITHER Question 2 “To what extent have you reduced your training volume due to injury,
#' illness or other health problems during the past week?” OR Question 3,
#' “To what extent has injury, illness or other health problems
#' affected your performance the past week?”.
#' This recipe has remained the same through all versions of the OSTRC-questionnaire,
#' and the function is compatible with OSTRC-O and OSTRC-H, version 1.0 and 2.0.
