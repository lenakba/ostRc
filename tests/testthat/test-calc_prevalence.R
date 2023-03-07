context("Calculate prevalence")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

# example data used in more than one test
d_ostrc = tribble(~id_participant, ~day_nr, ~hp,
                 1, 1, 1,
                 1, 1, 0,
                 1, 1, 0,
                 1, 2, 0,
                 1, 2, 1)

test_that("Returns a tibble with number of responses, number of cases, and prevalence.",
          {
            correct_columns = c("n_responses", "n_cases", "prev_cases")
            expect_true(all(correct_columns %in% names(
              calc_prevalence(d_ostrc$id_particpant, d_ostrc$day_nr, d_ostrc$hp)
            )))
          })

test_that("Returns correct number of responses.", {
  n_responses = c(3, 2)
  d_test = calc_prevalence(d_ostrc$id_particpant, d_ostrc$day_nr, d_ostrc$hp)
  expect_equal(d_test$n_responses, n_responses)
})

test_that("Returns correct number of cases (a response of 1 on an health problem variable is a case).",
          {
            n_cases = c(1, 1)
            d_test = calc_prevalence(d_ostrc$id_particpant, d_ostrc$day_nr, d_ostrc$hp)
            expect_equal(d_test$n_cases, n_cases)
          })

test_that("Returns correct prevalence (proportion).", {
  prevalence = c(0.5, 1 / 3)
  d_test = calc_prevalence(d_ostrc$id_particpant, d_ostrc$day_nr, d_ostrc$hp)
  expect_equal(d_test$prev_cases, prevalence)
})

test_that("Throws error if hp_type is not a binary integer.", {
  d_wrong_hp = d_ostrc %>% mutate(hp = 1:length(hp))
  expect_error(calc_prevalence(d_wrong_hp$id_particpant, d_wrong_hp$day_nr, d_wrong_hp$hp))
})

test_that("Throws error if time period only has 1 value.", {
  d_wrong_hp = d_ostrc %>% mutate(day_nr = rep(1, length(day_nr)))
  expect_error(calc_prevalence(d_wrong_hp$id_particpant, d_wrong_hp$day_nr, d_wrong_hp$hp))
})

test_that("Ignores time periods with missing values.", {
  id = rep(1, 5)
  time = c(NA, NA, 1, 2, 2)
  hp = c(1, 0, 0, 0, 1)

  n_responses = c(1, 2)
  n_cases = c(0, 1)

  d_test = calc_prevalence(id, time, hp)
  expect_equal(d_test$n_cases, n_cases)
  expect_equal(d_test$n_responses, n_responses)
})

test_that("Any input variable with only NAs will throw error.", {
  id = rep(1, 5)
  id_miss = rep(NA, 5)
  time = c(1, 1, 1, 2, 2)
  time_miss = rep(NA, 5)
  hp = c(1, 0, 0, 0, 1)
  hp_miss = rep(NA, 5)

  expect_error(calc_prevalence(id, time_miss, hp))
  expect_error(calc_prevalence(id, time, hp_miss))
  expect_error(calc_prevalence(id_miss, time_2, hp))
})
