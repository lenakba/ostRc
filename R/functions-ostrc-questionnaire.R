#' @importFrom magrittr %>%
#' @import tibble
#' @import dplyr
NULL

#' Standardize coding
#'
#' Function to standardize codes in OSTRC-questionnaire variables
#' to 0, 8, 17, and 25 for the corresponding replies of 1, 2, 3, 4 on each question.
#' Will only work if numeric input vector has exactly 4 unique values and is therefore
#' only compatible with OSTRC questionnaires version 2.0, and Q1 or Q4 from version 1.0.
#' The function automatically arranges these from smallest to largest and assigns
#' the smallest value to 0, the next smallest to 8, medium to 17, and highest value to 25.
#'
#' @param ostrc_q vector of class numeric with responses
#' to any of the 4 OSTRC-questionnaire questions in vesrion 2.0.
#' Or, responses to Q1 or Q4 from version 1.0.
#' @return a vector of class numeric with the standard codes of 0, 8, 17 or 25.
#' @examples
#' ostrc_q = c(0, 1, 2, 3, 2, 2, 3)
#' standardize_coding(ostrc_q)
#' @export
standardize_coding = function(ostrc_q){
  stopifnot(is.numeric(ostrc_q))

  non_zero_resps = na.omit(ostrc_q[ostrc_q != 0])
  if(all(non_zero_resps %in% c(8, 17, 25, 13, 19))){
    stop("All the OSTRC values are already coded as 0, 8, 17 or 25,
         or 0, 13, 17, 19, 25.")
  }

  unique_codes = na.omit(unique(ostrc_q))

  if(length(unique_codes) > 4){
    stop("There are more than 4 codes (corresponding to the 4 possible responses) in your vector.
       Is the input vector from Q2 or Q3 from OSTRC questionnaire version 1.0?
       This function is only compatible with version 2.0, and version 1.0 Q1 and Q4.
       Otherwise, maybe your missing data is coded as a number? Please convert these to NA.")
  }

  if(length(unique_codes) < 4){
    stop("There are fewer than 4 codes (corresponding to the 4 possible responses in version 2.0) in your vector.
       Perhaps no participant responded with a certain reply?
       Please convert vector to the standard 0, 8, 17, 25 responses manually.")
  }
  unique_codes_arranged = sort(unique_codes)
  pos_codes = match(ostrc_q, unique_codes_arranged)

  codes_wanted = c(0, 8, 17, 25)

  ostr_q_converted = codes_wanted[pos_codes]
  ostr_q_converted
}

#' Find health problems
#'
#' Function for finding health problems.
#' Returns a vector with binary values 1 health problem,
#' 0 for not a health problem.
#' Only responses to the first OSTRC question
#' "Have you had any difficulties participating in
#' training and competition due to (location) problems
#' during the past 7 days?" (version 2.0) or
#' “Have you had any difficulties participating in
#' normal training and competition due to injury,
#' illness or other health problems during the past week?" (version 1.0)
#' that were not “Full participation without health problems”
#' is considered a health problem.
#'
#' @param ostrc_1 vector of class numeric with values corresponding to
#'                responses in OSTRC questionnaire, question 1.
#' @return a vector of class numeric where 1 = health problem, 0 = not a health problem.
find_hp = function(ostrc_1){
  stopifnot(is.numeric(ostrc_1))

  non_zero_resps = na.omit(ostrc_1[ostrc_1 != 0])

  value_vec = c(8, 17, 25)
  warning_codes = paste0("One or more input vectors of OSTRC responses
                         had non-standard values (not in 0, 8, 17, 25).
                         Lowest value was assumed 0, highest value assumed 25,
                         before finding health problems.")
  warning_zeros = paste0("All of the responses are 0 or missing (NA),
                         meanining the function found no health problems.")

  if(!all(non_zero_resps %in% value_vec)){
    ostrc_1 = standardize_coding(ostrc_1)
    warning(warning_codes)
  }

  if(all(ostrc_1 == 0 | is.na(ostrc_1))){
    warning(warning_zeros)
  }
  ostrc_hp = ifelse(ostrc_1 > 0, 1, 0)
  ostrc_hp
}

#' Find substantial health problems
#'
#' Function to find substantial health problems given the definition
#' in the original OSTRC-O paper: doi.org/10.1136/bjsports-2012-091524.
#' Substantial health problems are defined as injury or illness
#' that required modified training volumes or performance to a moderate extent or worse,
#' or to a complete absence from sports.
#' Returns a vector with 0 for non-substantial/no health problem, 1 for substantial.
#' The function follows the OSTRC version 2.0 recipe of extracting substantial health problems
#' from the OSTRC questionnaire:
#' Any reply of "Could not participate due to (location) problems"
#' on Question 1: "Have you had any difficulties
#' participating in training and
#' competition due to (location) problems
#' during the past 7 days?" OR
#' a reply of “To a moderate extent” or “To a major extent"
#' on EITHER Question 2 OR Question 3:
#' “To what extent have you modified your training or competition
#' due to (location) problems during the past 7 days?”;
#' “To what extent has injury, illness or other health problems
#' affected your performance the past week?”.
#' In addition, for OSTRC questionnaires version 1.0,
#' a response of "Cannot participate at all"
#' on Question 2 and Question 3 is also considered substantial,
#' regardless of the response on Question 1.
#' The function is compatible with OSTRC-O and OSTRC-H, version 1.0 and 2.0.
#'
#' @param ostrc_1 vector of class numeric with responses to
#' OSTRC-questionnaire question 1: "Have you had any difficulties participating in
#' training and competition due to (location) problems during the past 7 days?".
#' @param ostrc_2 vector of class numeric with responses to
#' OSTRC-questionnaire question 2:
#' "To what extent have you modified your training or competition
#' due to (location) problems during the past 7 days?" (version 2.0)
#' OR “To what extent have you reduced your training volume
#' due to injury, illness or other health problems during the past week?” (version 1.0).
#' Standard response values are 0, 8, 17, 25 (version 2.0),
#' or 0, 13, 17, 19, 25 (version 1.0).
#' Note that the function assumes that input vectors
#' with values that are not the standard 0, 8, 17, 25
#' (or 0, 13, 17, 19, 25 for version 1.0)
#' are coded with the lowest value corresponding to 0
#' and highest value corresponding to 25.
#' An example would be a vector with codes 1, 2, 3, 4
#' for the 4 potential responses.
#' The function will throw an error if version 1.0
#' Q2 is coded with non-standard numeric codes.
#' These must be coded to 0, 13, 17, 19, 25 manually.
#' @param ostrc_3 vector of class numeric with responses to
#' OSTRC-questionnaire question 3:
#' "To what extent have (location) problems affected
#' your performance during the past 7 days?" (version 2.0) OR
#' “To what extent has injury, illness or other health problems
#' affected your performance the past week?” (version 1.0).
#' Standard response values are 0, 8, 17, 25 (version 2.0),
#' or 0, 13, 17, 19, 25 (version 1.0).
#' Note that the function assumes that input vectors
#' with values that are not the standard 0, 8, 17, 25
#' (or 0, 13, 17, 19, 25 for version 1.0)
#' are coded with the lowest value corresponding to 0
#' and highest value corresponding to 25.
#' An example would be a vector with codes 1, 2, 3, 4
#' for the 4 potential responses.
#' The function will throw an error if version 1.0
#' Q3 is coded with non-standard numeric codes.
#' These must be coded to 0, 13, 17, 19, 25 manually.
#' @param version String. Either "2.0" (Default) or "1.0".
#' @return a vector of class numeric with binary codes 1 for
#' substantial health problem, 0 for
#' no health problem or non-substantial health problem.
#' @examples
#'   ostrc_2 = c(0, 0, 0, 25)
#'   ostrc_3 = c(0, 0, 17, 0)
#'   find_hp_substantial(ostrc_2, ostrc_3)
#'
#'   ostrc_2_v1 = c(0, 0, 0, 19)
#'   ostrc_3_v1 = c(0, 0, 13, 0)
#'   find_hp_substantial(ostrc_2_v1, ostrc_3_v1)
#'
#'   ostrc_2_othercodes = c(1, 2, 3, 4)
#'   ostrc_3_othercodes = c(0, 1, 2, 3)
#'   find_hp_substantial(ostrc_2_othercodes, ostrc_3_othercodes)
#' @export
find_hp_substantial = function(ostrc_1, ostrc_2, ostrc_3, version = "2.0"){
  stopifnot(is.numeric(ostrc_1))
  stopifnot(is.numeric(ostrc_2))
  stopifnot(is.numeric(ostrc_3))

  if(all(is.na(c(ostrc_1, ostrc_2, ostrc_3))))
     stop("All input vectors consist of only missing values")

  non_zero_resps_1 = na.omit(ostrc_1[ostrc_1 != 0])
  non_zero_resps_2 = na.omit(ostrc_2[ostrc_2 != 0])
  non_zero_resps_3 = na.omit(ostrc_3[ostrc_3 != 0])

  value_vec = c(8, 17, 25, 13, 19)
  warning_obj = paste0("One or more input vectors of OSTRC responses
  had non-standard values (not in 0, 8, 17, 25 or 0, 13, 17, 19, 25).
  Lowest value was assumed 0, highest value assumed 25,
  before finding substantial health problems.")

  if(!all(non_zero_resps_1 %in% value_vec)){
    ostrc_1 = standardize_coding(ostrc_1)
    warning(warning_obj)
  }
  if(!all(non_zero_resps_2 %in% value_vec)){
    ostrc_2 = standardize_coding(ostrc_2)
    warning(warning_obj)
  }
  if(!all(non_zero_resps_3 %in% value_vec)){
    ostrc_3 = standardize_coding(ostrc_3)
    warning(warning_obj)
  }
  if(version == "2.0"){
    ostrc_sub = case_when(ostrc_1 == 25 | ostrc_2 >=17 | ostrc_3 >=17 ~ 1,
                          ostrc_1 < 25 & ostrc_2 <17 & ostrc_3 <17 ~ 0,
                          is.na(ostrc_1) & is.na(ostrc_2) & is.na(ostrc_3) ~ NA_real_)
  } else if(version == "1.0"){
    ostrc_sub = case_when(ostrc_2 >=13 | ostrc_3 >=13 ~ 1,
                          ostrc_2 <13 & ostrc_3 <13 ~ 0,
                          is.na(ostrc_2) & is.na(ostrc_3) ~ NA_real_)
  }
  ostrc_sub
}
