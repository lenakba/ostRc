context("Calculate mean prevalence per time period")
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
                  2, 2, 1,
                  3, 1, 0,
                  3, 2, 0)

test_that("Returns a tibble with the mean, sd and lower and upper ci.",
          {
            correct_columns = c("prev_mean", "prev_sd", "prev_ci_upper", "prev_ci_lower")
            expect_true(all(correct_columns %in% names(
              calc_prevalence_mean(d_ostrc, id_participant, day_nr, hp)
            )))
          })

test_that("Returns only 1 row of data - the mean for the given hp_type.", {
  d_test = calc_prevalence_mean(d_ostrc, id_participant, day_nr, hp)
  expect_equal(nrow(d_test), 1)
})

test_that("Calculates and returns correct mean and sd.", {

  d_test = calc_prevalence(d_ostrc, id_participant, day_nr, hp)
  test_mean = mean(d_test$prev_cases)
  test_sd = sd(d_test$prev_cases)

  d_prevmean = calc_prevalence_mean(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_prevmean$prev_mean, test_mean)
  expect_equal(d_prevmean$prev_sd, test_sd)
})


test_that("Ignores missing data when calculating mean.", {

  d_ostrc = tribble(~id_participant, ~day_nr, ~hp,
                    1, 1, 1,
                    1, 1, 1,
                    1, 2, NA,
                    2, 1, 1,
                    2, 2, NA)

  d_test = calc_prevalence(d_ostrc, id_participant, day_nr, hp)
  test_mean = mean(d_test$prev_cases, na.rm = TRUE)
  test_sd = sd(d_test$prev_cases, na.rm = TRUE)

  d_prevmean = calc_prevalence_mean(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_prevmean$prev_mean, test_mean)
  expect_equal(d_prevmean$prev_sd, test_sd)
})

test_that("CI lower is either lower or equal to mean,
          CI upper is greater than or equal to mean.", {
  d_prevmean = calc_prevalence_mean(d_ostrc, id_participant, day_nr, hp)
  expect_lte(d_prevmean$prev_mean, d_prevmean$prev_ci_lower)
  expect_gte(d_prevmean$prev_mean, d_prevmean$prev_ci_upper)
})
