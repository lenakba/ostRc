context("Add event id function")
library(ostrc)
library(lubridate)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

test_that("Returns correct days.", {
  d_matches_sim = tribble(~date, ~match,
                          "2017-08-22", 0,
                          "2017-08-23", 1,
                          "2017-08-24", 0,
                          "2017-08-25", 0,
                          "2017-08-26", 1)
  d_matches_sim = d_matches_sim %>% mutate(date = lubridate::as_date(date))
  cycle_days = as.character(c(1,1,2,2,2))
  expect_equal(add_event_id(d_matches_sim, date, match)$event_id, cycle_days)
})

test_that("Fills in missing dates and returns correct indices for those as well.", {
  d_matches_sim_missing = tribble(~date, ~match,
                                  "2017-08-22", 0,
                                  "2017-08-23", 1,
                                  "2017-08-25", 0,
                                  "2017-08-26", 1)
  d_matches_sim_missing = d_matches_sim_missing %>% mutate(date = lubridate::as_date(date))
  cycle_days = as.character(c(1,1,2,2,2))
  expect_equal(add_event_id(d_matches_sim_missing, date, match)$event_id, cycle_days)
})

test_that("Handles datasets with varying column names for date and match.", {
  d_matches_varnames = tribble(~training_date, ~match_indicator,
                               "2017-08-22", 0,
                               "2017-08-23", 1,
                               "2017-08-24", 0,
                               "2017-08-25", 0,
                               "2017-08-26", 1)
  d_matches_sim = tribble(~date, ~match,
                          "2017-08-22", 0,
                          "2017-08-23", 1,
                          "2017-08-24", 0,
                          "2017-08-25", 0,
                          "2017-08-26", 1)
  d_matches_varnames = d_matches_varnames %>%
    mutate(training_date = lubridate::as_date(training_date))
  d_matches_sim = d_matches_sim %>%
    mutate(date = lubridate::as_date(date))
  expect_equal(add_event_id(d_matches_varnames, training_date, match_indicator)$event_id,
               add_event_id(d_matches_sim, date, match)$event_id)
})

test_that("Returns error at incorrect datatypes.", {
  d_matches_datesasstrings = tribble(~date, ~match,
                                     "2017-08-22", 0,
                                     "2017-08-23", 1,
                                     "2017-08-24", 0)
  expect_error(add_event_id(d_matches_datesasstrings, date, match))
})


test_that("Will give correct indices for training days after last match and before first match.", {
  d_matches_noend = tribble(~date, ~match,
                            "2017-08-21", 0,
                            "2017-08-22", 0,
                            "2017-08-23", 1,
                            "2017-08-24", 0,
                            "2017-08-25", 0,
                            "2017-08-26", 0)
  d_matches_noend = d_matches_noend %>% mutate(date = lubridate::as_date(date))
  cycle_days = as.character(c(1,1,1,2,2,2))
  expect_equal(add_event_id(d_matches_noend, date, match)$event_id, cycle_days)
})
