#' @importFrom magrittr %>%
#' @importFrom rlang enquo as_string quo_name
#' @importFrom nplyr nest_mutate
#' @importFrom confintr ci_proportion
#' @import tibble
#' @import dplyr

NULL

#' Calculate prevalance
#'
#' A function to calculate the prevalence per given time period, such as per week.
#'
#' @param d_ostrc a dateframe with OSTRC questionnaire responses
#' @param id_participant vector within `d_ostrc` that identifies
#'                       a person, athlete, participant, etc.
#' @param time vector within `d_ostrc` that identifies a time period,
#'             such as a vector of dates or week-numbers. The prevalences will be calculated per
#'             value of this vector, such as per week.
#' @param hp_type a binary vector within `d_ostrc` which classifies a type of health problem as 1,
#'                and anything that is not the type of health problem as 0.
#'                This can be health problem (1/0), injury (1/0),
#'                illness (1/0), acute injury (1/0) or any other health problem type that the user wishes
#'                to calculate the prevalance on.
#' @export
calc_prevalence = function(d_ostrc, id_participant, time, hp_type){
  id_participant = enquo(id_participant)
  time = enquo(time)
  time_name = rlang::as_string(quo_name(time))
  hp_type = enquo(hp_type)
  hp_type_name = rlang::as_string(quo_name(hp_type))

  id_participant_values = d_ostrc %>% pull(!!id_participant)
  time_values = d_ostrc %>% pull(!!time)
  hp_type_values = d_ostrc %>% pull(!!hp_type)

  if(all(is.na(id_participant_values)) |
     all(is.na(time_values)) |
     all(is.na(hp_type_values))
  ) {
    stop("One of the input variables has only missing NA observations.")
  }

  if (!is.numeric(hp_type_values)) {
    stop(
      paste0(
        "Variable ",
        hp_type_name,
        " is not numeric or integer. Make sure ",
        hp_type_name,
        " is a binary variable of class numeric or integer."
      )
    )
  }

  if (length(unique(hp_type_values)) > 2) {
    stop(
      paste0(
        "Variable ",
        hp_type_name,
        " has more than two possible values. Make sure ",
        hp_type_name,
        " is a binary variable coded only with 0 and 1."
      )
    )
  }

  if (length(unique(time_values)) == 1) {
    stop(
      paste0(
        "Variable ",
        time_name,
        " has only one value. Are you sure this is the time period of interest?"
      )
    )
  }

  # Missing time points won't be included
  d_nonmissing = d_ostrc %>% filter(!is.na(!!time))

  # consider multiple health problems as just 1
  d_hp_type_per_id_per_time = d_nonmissing %>%
    group_by(!!id_participant, !!time) %>%
    summarise(hp_type_n = sum(!!hp_type, na.rm = TRUE)) %>%
    mutate(hp_type_atleast1 = ifelse(hp_type_n > 0, 1, 0)) %>%
    ungroup()

  # calculate prevalence
  d_prevalence = d_hp_type_per_id_per_time %>%
    group_by(!!time) %>%
    summarise(n_responses = n(),
              n_cases = sum(hp_type_atleast1, na.rm = TRUE),
              prev_cases = n_cases/n_responses)
  d_prevalence
}
