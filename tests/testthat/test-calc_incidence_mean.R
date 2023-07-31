context("Calculate mean incidence per time period")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

# example data used in more than one test
d_ostrc = tribble(~id_participant, ~day_nr, ~hp,
                  1, 1, 0,
                  1, 1, 1,
                  1, 2, 0,
                  1, 3, 1,
                  1, 4, 0,
                  2, 1, 0,
                  2, 2, 1,
                  2, 3, 0,
                  2, 4, 1,
                  3, 1, 0,
                  3, 2, 0,
                  3, 3, 1,
                  3, 3, 1,
                  4, 1, 1,
                  4, 2, 1,
                  4, 3, 0,
                  4, 4, 1)

test_that("Returns a tibble with the mean, sd and lower and upper ci.",
          {
            correct_columns = c("inc_mean", "inc_sd", "inc_ci_upper", "inc_ci_lower")
            expect_true(all(correct_columns %in% names(
              calc_incidence_mean(d_ostrc, id_participant, day_nr, hp)
            )))
          })

test_that("Returns only 1 row of data - the mean for the given hp_type.", {
  d_test = calc_incidence_mean(d_ostrc, id_participant, day_nr, hp)
  expect_equal(nrow(d_test), 1)
})

test_that("Calculates and returns correct mean and sd.", {

  d_test = calc_incidence(d_ostrc, id_participant, day_nr, hp)
  test_mean = mean(d_test$inc_cases, na.rm = TRUE)
  test_sd = sd(d_test$inc_cases, na.rm = TRUE)

  d_incmean = calc_incidence_mean(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_incmean$inc_mean, test_mean)
  expect_equal(d_incmean$inc_sd, test_sd)
})


test_that("Ignores missing data when calculating mean.", {

  d_ostrc_missing = tribble(~id_participant, ~day_nr, ~hp,
                            1, 1, 0,
                            1, 1, 1,
                            1, 2, NA,
                            2, 1, 0,
                            2, 2, NA,
                            3, 1, 0,
                            3, 2, 1,
                            3, 3, 0
  )

  d_test = calc_incidence(d_ostrc_missing, id_participant, day_nr, hp)
  test_mean = mean(d_test$inc_cases, na.rm = TRUE)
  test_sd = sd(d_test$inc_cases, na.rm = TRUE)

  d_incmean = calc_incidence_mean(d_ostrc_missing, id_participant, day_nr, hp)
  expect_equal(d_incmean$inc_mean, test_mean)
  expect_equal(d_incmean$inc_sd, test_sd)
})

test_that("CI lower is either lower or equal to mean,
          CI upper is greater than or equal to mean.", {
            d_incmean = calc_incidence_mean(d_ostrc, id_participant, day_nr, hp)
            expect_lte(d_incmean$inc_ci_lower, d_incmean$inc_mean)
            expect_gte(d_incmean$inc_ci_upper, d_incmean$inc_mean)
          })

test_that("Calculates and returns correct CIs.", {

  d_test = calc_incidence(d_ostrc, id_participant, day_nr, hp)
  count = nrow(d_test)
  se = sd(d_test$inc_cases) / sqrt(count)
  test_ci_lower = mean(d_test$inc_cases, na.rm = TRUE) - (qt(1 - ((1 - 0.95) / 2), count - 1) * se)
  test_ci_upper = mean(d_test$inc_cases, na.rm = TRUE) + (qt(1 - ((1 - 0.95) / 2), count - 1) * se)

  d_incmean = calc_incidence_mean(d_ostrc, id_participant, day_nr, hp)
  expect_equal(d_incmean$inc_ci_lower, test_ci_lower)
  expect_equal(d_incmean$inc_ci_upper, test_ci_upper)
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
  expect_warning(calc_incidence_mean(d_fewrows, id_participant, day_nr, hp))
})
