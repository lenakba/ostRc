context("Calculate prevalence")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

# example data used in more than one test
d_ostrc = tribble(~id_participant, ~day_nr, ~hp,
                 1, 1, 1,
                 1, 1, 1,
                 1, 2, 0,
                 2, 1, 1,
                 2, 2, 1)

test_that("Returns a tibble with number of responses, number of cases, and prevalence.",
          {
            correct_columns = c("n_responses", "n_cases", "prev_cases")
            expect_true(all(correct_columns %in% names(
              calc_prevalence(d_ostrc, id_participant, day_nr, hp)
            )))
          })

test_that("Returns correct number of responses.", {
  n_responses = c(2, 2)
  d_test = calc_prevalence(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_test$n_responses, n_responses)
})

test_that("Returns correct number of cases (a response of 1 on an health problem variable is a case).",
          {
            n_cases = c(2, 1)
            d_test = calc_prevalence(d_ostrc, id_participant, day_nr, hp)
            expect_equal(d_test$n_cases, n_cases)
          })

test_that("Returns correct prevalence (proportion).", {
  prevalence = c(1, 0.5)
  d_test = calc_prevalence(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_test$prev_cases, prevalence)
})

test_that("Considers multiple cases on the same individual per time period only once.",
          {
            d_multiple_hp = d_ostrc %>% mutate(hp = rep(1, length(hp)))
            n_cases = c(2, 2)

            d_test = calc_prevalence(d_multiple_hp, id_participant, day_nr, hp)
            expect_equal(d_test$n_cases, n_cases)
          })

test_that("Throws error if hp_type has more than 2 values.", {
  d_wrong_hp = d_ostrc %>% mutate(hp = 1:length(hp))
  expect_error(calc_prevalence(d_wrong_hp, id_participant, day_nr, hp))
})

test_that("Throws error if hp_type is not numeric or ingeteger.", {
  d_wrong_hp2 = tribble(~id_participant, ~day_nr, ~hp,
                    1, 1, "1",
                    1, 1, "ting",
                    1, 2, "0")
  expect_error(calc_prevalence(d_wrong_hp2, id_participant, day_nr, hp))
})

test_that("Throws error if time period only has 1 value.", {
  d_wrong_time = d_ostrc %>% mutate(day_nr = rep(1, length(day_nr)))
  expect_error(calc_prevalence(d_wrong_time, id_participant, day_nr, hp))
})

test_that("Ignores time periods with missing values.", {

  d_ostrc_miss = tribble(~id_participant, ~day_nr, ~hp,
                    1, NA, 1,
                    1, NA, 0,
                    1, 1, 0,
                    1, 2, 0,
                    1, 2, 1)

  n_responses = c(1, 1)
  n_cases = c(0, 1)

  d_test = calc_prevalence(d_ostrc_miss, id_participant, day_nr, hp)
  expect_equal(d_test$n_cases, n_cases)
  expect_equal(d_test$n_responses, n_responses)
})

test_that("Any input variable with only NAs will throw error.", {

  d_id_miss = tribble(~id_participant, ~day_nr, ~hp,
                         NA, 1, 1,
                         NA, 1, 0,
                         NA, 1, 0,
                         NA, 2, 0,
                         NA, 2, 1)

  d_time_miss = tribble(~id_participant, ~day_nr, ~hp,
                         1, NA, 1,
                         1, NA, 0,
                         1, NA, 0,
                         1, NA, 0,
                         1, NA, 1)

  d_hp_miss = tribble(~id_participant, ~day_nr, ~hp,
                         1, 1, NA,
                         1, 1, NA,
                         1, 1, NA,
                         1, 2, NA,
                         1, 2, NA)

  expect_error(calc_prevalence(d_id_miss, id_participant, day_nr, hp))
  expect_error(calc_prevalence(d_time_miss, id_participant, day_nr, hp))
  expect_error(calc_prevalence(d_hp_miss, id_participant, day_nr, hp))
})

test_that("Will remove observation from numerator and denominator if hp_type is missing.", {
  d_missing_hp = tribble(~id_participant, ~day_nr, ~hp,
                        1, 1, 1,
                        1, 1, NA,
                        1, 2, NA)

  d_test_res = tribble(~day_nr, ~n_responses, ~n_cases, ~prev_cases,
                         1, 1, 1, 1)

  d_res = calc_prevalence(d_missing_hp, id_participant, day_nr, hp)

  expect_equal(d_res, d_test_res)
})
