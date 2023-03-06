context("Find substantial injuries function")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

test_that("Returns 1 for substantial if EITHER of the given
          variables has a score of 17 or more.", {
  ostrc_1 = c(8, 8, 17, 17)
  ostrc_2 = c(0, 0, 0, 25)
  ostrc_3 = c(0, 0, 17, 0)
  correct_result = c(0, 0, 1, 1)

  expect_equal(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3), correct_result)
})

test_that("Returns 0 for non-substantial if there is a health problem,
          but not a substantial one.", {
            ostrc_1 = c(0, 8, 17, 17)
            ostrc_2 = c(0, 0, 0, 25)
            ostrc_3 = c(0, 0, 0, 0)
            correct_result = c(NA, 0, 0, 1)

            expect_equal(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3), correct_result)
          })

test_that("Returns NA if OSTRC_1 == 0, meaning no health problem at all.", {
            ostrc_1 = c(0, 8, 0, 17)
            ostrc_2 = c(0, 0, 0, 25)
            ostrc_3 = c(0, 0, 17, 0)
            correct_result = c(NA, 0, NA, 1)

    expect_equal(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3), correct_result)
  })

test_that("Returns error if all input values are NA.", {
  ostrc_1 = is.numeric(c(NA, NA, NA, NA))
  ostrc_2 = is.numeric(c(NA, NA, NA, NA))
  ostrc_3 = is.numeric(c(NA, NA, NA, NA))
  expect_error(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3))
})

test_that("Returns NA if all three vectors are NA simultaneously.", {
  ostrc_1 = c(8, NA, NA, NA)
  ostrc_2 = c(17, NA, NA, NA)
  ostrc_3 = c(25, NA, NA, NA)
  correct_results = c(1, NA, NA, NA)
  expect_equal(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3), correct_results)
})

test_that("Returns 1 even if only one value is substantial and non-missing,
          but returns NA if only one value is non-missing
          and is not substantial.", {
  ostrc_1 = c(8, NA, NA, NA)
  ostrc_2 = c(NA, 8, NA, NA)
  ostrc_3 = c(NA, NA, 25, NA)
  correct_result = c(NA, NA, 1, NA)
  expect_equal(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3), correct_result)

  ostrc_4 = c(0, NA, NA, 8)
  ostrc_5 = c(NA, 0, NA, NA)
  ostrc_6 = c(NA, NA, 0, NA)
  correct_result = as.numeric(c(NA, NA, NA, NA))
  expect_equal(find_hp_substantial(ostrc_4, ostrc_5, ostrc_6), correct_result)
})

test_that("Returns error if one of the OSTRC variables are non-numeric.", {
  ostrc_1 = c(TRUE, FALSE, FALSE, TRUE)
  ostrc_2 = c(0, 8, 17, 25)
  ostrc_3 = c("Reduced participation", "Feeling pain",
              "Reduced participation", "Feeling great")

  expect_error(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3))
  expect_error(find_hp_substantial(ostrc_2, ostrc_2, ostrc_3))
})

test_that("Finds substantial health problems for the 1.0 version of the questionnaire.", {
  ostrc_1 = c(8, 8, 17, 17)
  ostrc_2 = c(0, 0, 0, 19)
  ostrc_3 = c(0, 0, 13, 0)
  correct_result = c(0, 0, 1, 1)

  expect_equal(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3, version = "1.0"), correct_result)
})

test_that("Throws warning if OSTRC_1 is 0 (no health problem)
          or 8 (no reduced performance or participation)
          while OSTRC 2 or 3 is substantial,
          which can happen in 1.0 version of the questionnaire.", {
  ostrc_1 = c(0, 0, 8, 0)
  ostrc_2 = c(0, 0, 0, 19)
  ostrc_3 = c(0, 0, 25, 0)

  expect_warning(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3, version = "1.0"))
})

test_that("Will handle OSTRC variables not coded in the classic 0,8,17,25 responses,
          but throw a warning.", {
            ostrc_1 = c(1, 2, 3, 4)
            ostrc_2 = c(0, 1, 2, 3)
            ostrc_3 = c(1, 2, 3, 4)
            correct_result1 = c(NA, 0, 1, 1)

            ostrc_4 = c(0, 0, 3, 3)
            ostrc_5 = c(3, 3, 0, 0)

            expect_equal(suppressWarnings(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3)), correct_result1)
            expect_error(suppressWarnings(find_hp_substantial(ostrc_1, ostrc_4, ostrc_5)))
            expect_warning(find_hp_substantial(ostrc_1, ostrc_2, ostrc_3))
})

