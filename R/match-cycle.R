
# This script has a simple function for calculating Acute: Chronic Workload Ratios (ACWR).
# It is desgined based on the most common defintions of ACWR.
# The function is not designed to calculate any and every possible method
# for calculating ACWR.

#' @importFrom magrittr %>%
#' @importFrom rlang as_string quo_name enquo
#' @import tibble
#' @import dplyr
NULL

#' Function to add an index to an existing training-load dataset
#' for each match-cycle. That is, all days up to first match in one cycle,
#' all days from first match to second match in the next cycle and so on.
#' Requires a data-variable and an indicator for match
#'
#' @param d A dataset with dates and a match-indicator. Results will append to given dataset.
#' @param date Each date training load was collected. Requires to be of class Date. Does not need to
#'             be non-missing
#' @param match An indicator-variable that states 1 for a match-day, 0 for non-match.
#' @export
add_match_cycle = function(d, date, match){
  date = enquo(date)
  date_name = as_string(quo_name(date))

  # index for days until match
  index_distinct = d %>%
    filter({{match}} == 1 | !!date == max(!!date)) %>%
    arrange(!!date) %>%
    distinct(!!date) %>%
    rownames_to_column()

  # some dates might be missing. Extracting dates and completing them
  dates = d %>% pull(!!date)
  dates = seq.Date(min(dates), max(dates), by ="day") %>% as_tibble()
  names(dates) = date_name

  # add match-index for completed dates
  d_index = dates %>% left_join(index_distinct, by = date_name)

  # add match-index and dates to desired dataset
  d_matches = d %>% full_join(d_index, by = date_name) %>% arrange(!!date)

  # fill missing day-indices
  d_matches = d_matches %>% rename(match_cycle = rowname) %>% fill(match_cycle, .direction = "up")
  d_matches
}
