#' @importFrom magrittr %>%
#' @import tibble
#' @import dplyr

#' Function to standardize codes in OSTRC-questionnaire variables
#' to 0, 8, 17, and 25 for the corresponding replies of 1, 2, 3, 4 on each question.
#' Will only work if numeric input vector has exactly 4 unique values.
#' The function automatically arranges these from smallest to largest and assigns
#' the smallest value to 0, the next smallest to 8, medium to 17, and highest value to 25.
#'
#' @param ostrc_q vector of class numeric with responses to any of the 4 OSTRC-questionnaire questions.
#' @return a vector of class numeric with the standard codes of 0, 8, 17 or 25.
#' @examples
#' ostrc_q = c(0, 1, 2, 3, 2, 2, 3)
#' standardize_coding(ostrc_q)
#' @export
standardize_coding = function(ostrc_q){
  stopifnot(is.numeric(ostrc_q))

  non_zero_resps = ostrc_q[ostrc_q != 0]
  if(all(non_zero_resps %in% c(8, 17, 25))){
    stop("All the OSTRC values are already coded as 0, 8, 17 or 25.")
  }

  unique_codes = unique(ostrc_q)

  if(length(unique_codes) >4){
    stop("There are more than 4 codes (corresponding to the 4 possible responses) in your vector.
       Maybe your missing data is coded as a number? Please convert these to NA.")
  }

  if(length(unique_codes) <4){
    stop("There are fewer than 4 codes (corresponding to the 4 possible responses) in your vector.
       Perhaps no participant responded with a certain reply?
       Please convert vector to the standard 0, 8, 17, 25 responses manually.")
  }
  unique_codes_arranged = sort(unique_codes)
  pos_codes = match(ostrc_q, unique_codes_arranged)

  codes_wanted = c(0, 8, 17, 25)

  ostr_q_converted = codes_wanted[pos_codes]
  ostr_q_converted
}

#' Only responses to the first OSTRC question
#' (“Have you had any difficulties participating in
#' normal training and competition due to injury,
#' illness or other health problems during the past week?”)
#' that were not “Full participation without health problems”
#' were considered to be indicative of a health problem.


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
