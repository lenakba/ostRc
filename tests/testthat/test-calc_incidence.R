context("Calculate incidence")
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
                  1, 3, 1,
                  2, 1, 1,
                  2, 2, 0,
                  2, 3, 1)

test_that("Returns a tibble with number of responses, number of cases, and incidence.",
          {
            correct_columns = c("n_responses", "n_new_cases", "inc_cases")
            expect_true(all(correct_columns %in% names(
              calc_incidence(d_ostrc, id_participant, day_nr, hp)
            )))
          })

test_that("Returns correct number of responses.", {
  n_responses = c(2, 2, 2)
  d_test = calc_incidence(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_test$n_responses, n_responses)
})

test_that("Returns correct number of cases (a response of 1 on an health problem variable is a case,
          IF there was a 0 on the previous timepoint.
          IF no participant has data on the previous timepoint, it will be missing.).",
          {
            n_new_cases = c(NA, 0, 2)
            d_test = calc_incidence(d_ostrc, id_participant, day_nr, hp)
            expect_equal(d_test$n_new_cases, n_new_cases)
          })

test_that("Returns correct incidence (proportion).", {
  incidence = c(NA, 0, 1)
  d_test = calc_incidence(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_test$inc_cases, incidence)
})

test_that("Considers multiple cases on the same individual per time period only once.",
          {
            d_multiple_hp = tribble(~id_participant, ~day_nr, ~hp,
                                    1, 1, 0,
                                    1, 2, 1,
                                    1, 2, 1,
                                    1, 3, 1,
                                    2, 1, 1,
                                    2, 2, 0,
                                    2, 3, 1)
            n_new_cases = c(NA, 1, 1)
            d_test = calc_incidence(d_multiple_hp, id_participant, day_nr, hp)
            expect_equal(d_test$n_new_cases, n_new_cases)
          })

test_that("If timepoint on previous week only has missing data, a case result of 1 is consiered NA.
           However, a 0 is still considered a 0.
           If all cases are 0 on the first timepoint, we can be sure incidence is 0%.
           If some replies are 1, we cannot know for sure what the incidence is,
           because we do not have info on previous week.", {

  d_ostrc_0atstart = tribble(~id_participant, ~day_nr, ~hp,
                              1, 1, 0,
                              1, 2, 0,
                              1, 3, 1,
                              2, 1, 0,
                              2, 2, 0,
                              2, 3, 1)
  n_new_cases_0atstart = c(0, 0, 2)
  d_test_0atstart = calc_incidence(d_ostrc_0atstart, id_participant, day_nr, hp)
  expect_equal(d_test_0atstart$n_new_cases, n_new_cases_0atstart)

  d_ostrc_1atstart = tribble(~id_participant, ~day_nr, ~hp,
                             1, 1, 1,
                             1, 2, 0,
                             1, 3, 1,
                             2, 1, 0,
                             2, 2, 0,
                             2, 3, 1)
  n_new_cases_1atstart = c(NA, 0, 2)
  d_test_1atstart = calc_incidence(d_ostrc_1atstart, id_participant, day_nr, hp)
  expect_equal(d_test_1atstart$n_new_cases, n_new_cases_1atstart)
})

test_that("Throws error if hp_type has any other values than 0, 1 or NA.", {
  d_wrong_hp = d_ostrc %>% mutate(hp = 1:length(hp))
  expect_error(calc_incidence(d_wrong_hp, id_participant, day_nr, hp))
})

test_that("Throws error if hp_type is not numeric or integer.", {
  d_wrong_hp2 = tribble(~id_participant, ~day_nr, ~hp,
                        1, 1, "1",
                        1, 1, "ting",
                        1, 2, "0")
  expect_error(calc_incidence(d_wrong_hp2, id_participant, day_nr, hp))
})

test_that("Throws error if time period only has 1 value.", {
  d_wrong_time = d_ostrc %>% mutate(day_nr = rep(1, length(day_nr)))
  expect_error(calc_incidence(d_wrong_time, id_participant, day_nr, hp))
})

test_that("Ignores time periods with missing values.", {

  d_ostrc_miss = tribble(~id_participant, ~day_nr, ~hp,
                         1, NA, 1,
                         1, NA, 0,
                         1, 1, 0,
                         1, 2, 0,
                         1, 2, 1)

  n_responses = c(1, 1)
  n_new_cases = c(0, 1)

  d_test = calc_incidence(d_ostrc_miss, id_participant, day_nr, hp)
  expect_equal(d_test$n_new_cases, n_new_cases)
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

  expect_error(calc_incidence(d_id_miss, id_participant, day_nr, hp))
  expect_error(calc_incidence(d_time_miss, id_participant, day_nr, hp))
  expect_error(calc_incidence(d_hp_miss, id_participant, day_nr, hp))
})

test_that("Will remove observation from numerator and denominator if hp_type is missing.", {
  d_missing_hp = tribble(~id_participant, ~day_nr, ~hp,
                         1, 1, NA,
                         1, 1, NA,
                         1, 2, 1)

  d_test_res = tribble(~day_nr, ~n_responses, ~n_new_cases, ~inc_cases,
                       2, 1, NA, NA) %>%
                       mutate(n_new_cases = as.numeric(n_new_cases),
                              inc_cases = as.numeric(inc_cases))

  d_res = suppressWarnings(calc_incidence(d_missing_hp, id_participant, day_nr, hp))

  expect_equal(d_res, d_test_res)
})

test_that("Throws warning if incidence is constant.", {
  d_fewrows = tribble(~id_participant, ~day_nr, ~hp,
                      1, 1, 1,
                      1, 2, 1,
                      1, 3, 1,
                      2, 1, 1,
                      2, 2, 1,
                      3, 1, 1,
                      3, 2, 1)
  expect_warning(calc_incidence(d_fewrows, id_participant, day_nr, hp))
})
