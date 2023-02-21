#' @importFrom magrittr %>%
#' @importFrom rlang as_string quo_name enquo
#' @importFrom slider slide_index
#' @importFrom purrr map compact
#' @import tibble
#' @import dplyr
NULL

#' Add Event Id
#'
#' Function to find intervals of unequal lengths for each event in the data.
#' The function adds a column indexing each interval.
#' This is useful for finding intervals between multiple injuries or
#' episodes of pain on the same individual. Or the opposite:
#' the intervals where the participant is currently injured or in pain.
#' It can also be used to find the
#' so-called micro-cycle between matches and competition events. That is,
#' all days up to first match/competition in one cycle,
#' all days from first match to second match in the next cycle and so on.
#' Requires a date-variable and an indicator for the event (injury/match/competition).
#'
#' @param d A dataset with dates and an indicator for the event. Results will append to given dataset
#' @param date Each date of data collection. I.e. date of OSTRC-response, or date of training load collected.
#'             Must be of class Date. If some dates are missing (e.g. no data collection that day),
#'             these will be added in automatically.
#' @param event An indicator-variable that states 1 for a event-day, 0 for non-event.
#'              An event is typically an injury or competition/match.
#'              Can be any event that is of interest to the study.
#' @return The input dataset with a new column, which indexes each event interval.
#' @examples
#' d_matches_sim = tribble(~date, ~match,
#'                         "2017-08-22", 0,
#'                         "2017-08-23", 1,
#'                         "2017-08-24", 0,
#'                         "2017-08-25", 0,
#'                         "2017-08-26", 1)
#' d_matches_sim = d_matches_sim %>% mutate(date = lubridate::as_date(date))
#' add_event_id(d_matches_sim, date, match)
#' @export
add_event_id = function(d, date, event){
  date = enquo(date)
  date_name = as_string(quo_name(date))
  id = as_string(quo_name(id))

  # index for days until event
  index_distinct = d %>%
    filter({{event}} == 1 | !!date == max(!!date)) %>%
    arrange(!!date) %>%
    distinct(!!date) %>%
    rownames_to_column()

  # some dates might be missing. Extracting dates and completing them
  dates = d %>% pull(!!date)
  dates = seq.Date(min(dates), max(dates), by ="day") %>% enframe(name = NULL)
  names(dates) = date_name

  # add event-index for completed dates
  d_index = dates %>% left_join(index_distinct, by = date_name)

  # add event-index and dates to desired dataset
  d_events = d %>% full_join(d_index, by = date_name) %>% arrange(!!date)

  # fill missing day-indices
  d_events = d_events %>%
    rename(event_id = rowname) %>%
    fill(event_id, .direction = "up")
  d_events
}
