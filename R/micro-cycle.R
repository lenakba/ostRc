#' @importFrom magrittr %>%
#' @importFrom rlang as_string quo_name enquo
#' @importFrom slider slide_index
#' @importFrom purrr map compact
#' @import tibble
#' @import dplyr
NULL

#' Add Micro Cycle
#'
#' Function to add an index describing an interval between two competition or match events,
#' also known as a match/competition micro-cycle. That is, all days up to first match
#' in one cycle,all days from first match to second match in the next cycle and so on.
#' Can also be used to create intervals between injuries or other events.
#' Requires a date-variable and an indicator for the event (match/competition).
#'
#' @param d A dataset with dates and an indicator for the event. Results will append to given dataset
#' @param date Each date training load was collected. Requires to be of class Date. Does not need to
#'             be non-missing
#' @param event An indicator-variable that states 1 for a event-day, 0 for non-event.
#'              An event is typically a competition or match.
#' @export
add_micro_cycle = function(d, date, event){
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
    rename(micro_cycle = rowname) %>%
    fill(micro_cycle, .direction = "up")
  d_events
}
