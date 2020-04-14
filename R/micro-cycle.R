
# This script has a simple function for calculating Acute: Chronic Workload Ratios (ACWR).
# It is desgined based on the most common defintions of ACWR.
# The function is not designed to calculate any and every possible method
# for calculating ACWR.

#' @importFrom magrittr %>%
#' @importFrom rlang as_string quo_name enquo
#' @importFrom slider slide_index
#' @importFrom purrr map compact
#' @import tibble
#' @import dplyr
NULL

#' Add Micro Cycle
#'
#' Function to add an index to an existing training-load dataset
#' for each micro-cycle between matches. That is, all days up to first match in one cycle,
#' all days from first match to second match in the next cycle and so on.
#' Can also be used to create intervals between injuries or other events.
#' Requires a data-variable and an indicator for match
#'
#' @param d A dataset with dates and an indicator for the event. Results will append to given dataset
#' @param date Each date training load was collected. Requires to be of class Date. Does not need to
#'             be non-missing
#' @param event An indicator-variable that states 1 for a event-day, 0 for non-event.
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
  d_matches = d %>% full_join(d_index, by = date_name) %>% arrange(!!date)

  # fill missing day-indices
  d_matches = d_matches %>% rename(match_cycle = rowname) %>% fill(match_cycle, .direction = "up")
  d_matches
}

#' Micro-Cycle Based Chronic Loads
#'
#' Function for calculating chronic loads for every X number of match cycles in a sliding window.
#' The final index does not indicate a single match_cycle, but is rather an index for each chronic window
#' EWMA is the method of calculations. Missing values are removed from each chronic window before calculation.
#'
#' @param d dataframe with load and micro-cycle columns
#' @param load Name of variable in dataset with training load values
#' @param micro_cycle Name of variable in dataset with index for the micro-cycle the load value belongs to. This will often be the match-period,
#'                    but any index, either of irregular length or not (eg. weeks) will work.
#' @param n_chronic_cycles The number of cycles, in addition to the first, that is to be included in the chronic time period.
#' Example, n_chronic_cycles = 2 will calculate EWMA over 1 + 2 = 3 cycles. Defaults to 2. Needs to be atleast 1.
#' @export
micro_cycle_cl = function(d, load, micro_cycle, n_chronic_cycles = 2){

  micro_cycle = enquo(micro_cycle)
  load = enquo(load)

  load_length = d %>% summarise(load_length = length(!!load)) %>% pull(load_length)

  if(load_length<=2){
    stop(paste0("Error: Load vector length too short for sliding windows. Length = ",load_length,"."))
  }

  n_load = nrow(d)
  n_cycles = n_distinct(d %>% select(!!micro_cycle))

  if(n_load <= n_cycles){
    warning("Warning: All cycles have only 1 element.")
  }

  # create sliding index
  chronic_loads_indices_duplicated  = d %>%
                                      transmute(chronic_windows =
                                                slide_index(!!load, !!micro_cycle, ~.x, .before = 0, .after = n_chronic_cycles, .complete =TRUE))
  # remove duplicated chronic indices and null
  chronic_loads_indices = chronic_loads_indices_duplicated %>% map(~unique(.))
  chronic_loads_indices_no_null = compact(chronic_loads_indices$chronic_windows)

  # calculate EWMA and extract last value in each chronic period
  chronic_loads = chronic_loads_indices_no_null %>%
                  map(~ewma(na.omit(.x), n_days = length(na.omit(.x)), window = 1)) %>%
                  map(~last(.))

  # collapse to dataframe
  d_chronic = unlist(chronic_loads) %>% enframe(name = "index", value = "chronic_load")
  d_chronic
}

#' Micro-Cycle Based Acute Loads
#'
#' Function for calculating the mean load of each given mico cycle. Returns a dataframe with an index for each cycle and the acute load.
#'
#' @param d dataframe with load and micro-cycle columns
#' @param load Name of variable in dataset with training load values
#' @param micro_cycle Name of variable in dataset with index for the micro-cycle the load value belongs to. This will often be the match-period,
#'                    but any index, either of irregular length (e.g. match periods) or not (e.g. weeks) will work.
#' @export
micro_cycle_al = function(d, load, micro_cycle){

  micro_cycle = enquo(micro_cycle)
  load = enquo(load)

  d = d %>%
    group_by(!!micro_cycle) %>%
    mutate(acute_load = mean(!!load, na.rm = TRUE)) %>%
    ungroup
  d_acute = d %>% distinct(!!micro_cycle, .keep_all = TRUE) %>% select(!!micro_cycle, acute_load)
  d_acute
}

#' Micro-Cycle Based Acute-Chronic Workload Ratio (ACWR)
#'
#' Function to calculate ACWR based on a dataset that includes the acute and chronic loads.
#' The function assumes an uncoupled ACWR.
#'
#' @param d dataframe with acute and chronic loads as a result of micro_cycle_al and micro_cycle_cl.
#' @param n_chronic_cycles The number of cycles included in the chronic load - 1,
#'                         which is used to determine how many places acute needs to be pushed to
#'                         be calculated the correct chronic load. Defaults to 2, which is 3 cycles in the chronic load.
#' @export
micro_cycle_acwr = function(d_acute_chronic, n_chronic_cycles = 2){

  d_acute_chronic = d_acute_chronic %>%
    mutate(acute_load_uc = lead(acute_load, n_chronic_cycles),
           acwr = acute_load_uc/chronic_load)
  d_acute_chronic

}

#' Micro-Cycle Based Loads
#'
#' Helper function that combines the power of micro_cycle_al, micro_cycle_cl and micro_cycle_acwr.
#' Calculates acute and chronic loads based on micro cycles, combines the two and finally calculates ACWR.
#'
#' @param d dataframe with load and micro-cycle columns
#' @param load Name of variable in dataset with training load values
#' @param micro_cycle Name of variable in dataset with index for the micro-cycle the load value belongs to. This will often be the match-period,
#'                    but any index, either of irregular length or not (eg. weeks) will work.
#' @param n_chronic_cycles The number of cycles, in addition to the first, that is to be included in the chronic time period.
#' Example, n_chronic_cycles = 2 will calculate EWMA over 1 + 2 = 3 cycles. Defaults to 2. Needs to be atleast 1.
#' @export
micro_cycle_loads = function(d, load, micro_cycle, n_chronic_cycles = 2){

  load = enquo(load)
  micro_cycle = enquo(micro_cycle)

  d_cl =  micro_cycle_cl(d, !!load, !!micro_cycle, n_chronic_cycles)
  d_al =  micro_cycle_al(d, !!load, !!micro_cycle)
  d_acute_chronic = d_cl %>% left_join(d_al, by = c("index" = "match_cycle"))
  d_acute_chronic = micro_cycle_acwr(d_acute_chronic, n_chronic_cycles)
  d_acute_chronic
}
