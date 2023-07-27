context("Calculate mean prevalence for all given hp_types and groups")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

# example data used in more than one test
d_ostrc = tribble(~id_participant, ~day_nr, ~hp, ~hp_sub, ~season,
                  1, 1, 1, 0, 1,
                  1, 2, 1, 1, 1,
                  1, 3, 0, 0, 1,
                  2, 1, 1, 1, 1,
                  2, 2, 1, 1, 1,
                  3, 1, 0, 0, 1,
                  3, 2, 0, 0, 1,
                  1, 1, 1, 0, 2,
                  1, 2, 1, 0, 2,
                  1, 3, 0, 0, 2,
                  2, 1, 1, 0, 2,
                  2, 2, 1, 0, 2,
                  3, 1, 1, 1, 2,
                  3, 2, 1, 1, 2
                  )

test_that("Returns a tibble with the hp_type, mean, sd and lower and upper ci.",
          {
            correct_columns = c("hp_type", "prev_mean", "prev_sd", "prev_ci_upper", "prev_ci_lower")
            expect_true(all(correct_columns %in% names(
              calc_prevalence_all(d_ostrc, id_participant, day_nr, c("hp", "hp_sub"))
            )))
          })

test_that("Returns 1 row of data per hp_type.", {
  hp_type_vector = c("hp", "hp_sub")
  n_types = length(hp_type_vector)
  d_test = calc_prevalence_all(d_ostrc, id_participant, day_nr, hp_type_vector)
  expect_equal(nrow(d_test), n_types)
})

#---------------Tests for group option

test_that("Returns the same name on the group column as the one given.", {

  correct_column_names = c("season", "hp_type", "prev_mean", "prev_sd", "prev_ci_upper", "prev_ci_lower")
  hp_type_vector = c("hp", "hp_sub")
  test_names = names(calc_prevalence_all(d_ostrc, id_participant, day_nr, hp_type_vector, "season"))

  expect_true(all(correct_column_names %in% test_names))
})

test_that("Group column is the first group provided.", {

  correct_column_names = c("season", "hp_type", "prev_mean", "prev_sd", "prev_ci_upper", "prev_ci_lower")
  hp_type_vector = c("hp", "hp_sub")
  test_names = names(calc_prevalence_all(d_ostrc, id_participant, day_nr, hp_type_vector, "season"))

  expect_true(all(correct_column_names %in% test_names))
})

test_that("Returns 1 row of data per hp_type, per category in the given group.", {
   hp_type_vector = c("hp", "hp_sub")
   n_types = length(hp_type_vector)
   n_cats = length(unique(d_ostrc$season))
   n_rows_expected = n_cats*n_types

   d_test = calc_prevalence_all(d_ostrc, id_participant, day_nr, hp_type_vector, "season")
   expect_equal(nrow(d_test), n_rows_expected)
 })

test_that("The group is the first column in the returned data.", {
  hp_type_vector = c("hp", "hp_sub")
  d_test = calc_prevalence_all(d_ostrc, id_participant, day_nr, hp_type_vector, "season")
  pos = which(names(d_test)=="season")
  expect_equal(pos, 1)
})

test_that("The function provides 0 for groups that do not have any health problems, even though other groups do.", {
  hp_type_vector = c("hp", "hp_sub")

  d_ostrc = tribble(~id_participant, ~day_nr, ~hp, ~hp_sub, ~season,
                    1, 1, 1, 0, 1,
                    1, 2, 1, 1, 1,
                    1, 3, 0, 0, 1,
                    2, 1, 1, 1, 1,
                    2, 2, 1, 1, 1,
                    3, 1, 0, 0, 1,
                    3, 2, 0, 0, 1,
                    1, 1, 1, 0, 2,
                    1, 2, 1, 0, 2,
                    1, 3, 0, 0, 2,
                    2, 1, 1, 0, 2,
                    2, 2, 1, 0, 2,
                    3, 1, 1, 0, 2,
                    3, 2, 1, 0, 2
  )

  d_test = calc_prevalence_all(d_ostrc, id_participant, day_nr, hp_type_vector, "season")
  d_test_filtered = d_test %>% filter(season == 2, hp_type == "hp_sub")
  expect_equal(d_test_filtered$prev_mean, 0)
})
