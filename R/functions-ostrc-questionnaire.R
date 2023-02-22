#' @importFrom magrittr %>%
#' @import tibble
#' @import dplyr
NULL

#' Standardize coding
#'
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

#'Find substantial health problems
#'
#' Function to find substantial health problems given the definition
#' in the original OSTRC-O paper: doi.org/10.1136/bjsports-2012-091524.
#' Returns a vector with 0 for non-substantial/no health problem, 1 for substantial.
#' Substantial health problems are defined as injury or illness
#' that reduced training volumes or performance to a moderate extent or worse,
#' or to a complete absence from sports.
#' The function follows the original recipe of extracting substantial health problems
#' from the OSTRC questionnaire:
#' any reply of “To a moderate extent”, “To a major extent” or “Cannot participate at all”,
#' on EITHER Question 2 “To what extent have you reduced your training volume due to injury,
#' illness or other health problems during the past week?” OR Question 3,
#' “To what extent has injury, illness or other health problems
#' affected your performance the past week?”.
#' This recipe has remained the same through all versions of the OSTRC-questionnaire,
#' and the function is compatible with OSTRC-O and OSTRC-H, version 1.0 and 2.0.
#'
#' @param ostrc_2 vector of class numeric with responses to OSTRC-questionnaire question 2:
#' “To what extent have you reduced your training volume due to injury, illness or
#' other health problems during the past week?”. Standard response values are 0, 8, 17, 25.
#' Note that the function assumes that input vectors with 4 unique values that are not 0, 8, 17, 25,
#' (i.e. have the codes 1, 2, 3, 4 for each of the 4 possible responses),
#' are coded with the lowest value corresponding to 0 and highest value corresponding to 25.
#' @param ostrc_3 vector of class numeric with responses to OSTRC-questionnaire question 3:
#' “To what extent has injury, illness or other health problems
#' affected your performance the past week?”
#' #' Note that the function assumes that input vectors with 4 unique values that are not 0, 8, 17, 25,
#' (i.e. have the codes 1, 2, 3, 4 for each of the 4 possible responses instead),
#' are coded with the lowest value corresponding to 0 and highest value corresponding to 25.
#' @return a vector of class numeric with binary codes 1 for substantial health problem, 0 for
#' no health problem or non-substantial health problem.
#' @examples
#'   ostrc_2 = c(0, 0, 0, 25)
#'   ostrc_3 = c(0, 0, 17, 0)
#'   find_substantial(ostrc_2, ostrc_3)
#' @export
find_inj_substantial = function(ostrc_2, ostrc_3){
  stopifnot(is.numeric(ostrc_2))
  stopifnot(is.numeric(ostrc_3))

  non_zero_resps_2 = ostrc_2[ostrc_2 != 0]
  non_zero_resps_3 = ostrc_3[ostrc_3 != 0]

  if(!all(non_zero_resps_2 %in% c(8, 17, 25))){
    ostrc_2 = standardize_coding(ostrc_2)
    warning("The input vector of OSTRC question 2 responses had non-standard values.
            These were standardized, from lowest to highest value, to 0, 8, 17, 25, respectively,
            before finding substantial injuries.")
  }
  if(!all(non_zero_resps_3 %in% c(8, 17, 25))){
    ostrc_3 = standardize_coding(ostrc_3)
    warning("The input vector of OSTRC question 3 responses had non-standard values.
            These were standardized, from lowest to highest value, to 0, 8, 17, 25, respectively,
            before finding substantial injuries.")
  }
  ostrc_sub = ifelse(ostrc_2 >=17 | ostrc_3 >=17, 1, 0)
  ostrc_sub
}

#' Only responses to the first OSTRC question
#' (“Have you had any difficulties participating in
#' normal training and competition due to injury,
#' illness or other health problems during the past week?”)
#' that were not “Full participation without health problems”
#' were considered to be indicative of a health problem.
