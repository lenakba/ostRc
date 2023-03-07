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
                  2, 2, 1)

test_that("Returns a tibble with the mean, sd and lower and upper ci.",
          {
            correct_columns = c("prev_mean", "prev_sd", "prev_ci_upper", "prev_ci_lower")
            expect_true(all(correct_columns %in% names(
              calc_prevalence_mean(d_ostrc, id_participant, day_nr, hp)
            )))
          })
