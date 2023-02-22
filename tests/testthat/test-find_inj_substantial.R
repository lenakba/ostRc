context("Find substantial injuries function")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

test_that("Returns 1 for substantial if EITHER of the given variables has a score of 17 or more.", {
  ostrc_2 = c(0, 0, 0, 25)
  ostrc_3 = c(0, 0, 17, 0)
  correct_result = c(0, 0, 1, 1)

  expect_equal(find_inj_substantial(ostrc_2, ostrc_3), correct_result)
})

test_that("Returns error if one of the OSTRC variables are non-numeric.", {
  ostrc_1 = c(TRUE, FALSE, FALSE, TRUE)
  ostrc_2 = c(0, 8, 17, 25)
  ostrc_3 = c("Reduced participation", "Feeling pain",
              "Reduced participation", "Feeling great")

  expect_error(find_inj_substantial(ostrc_1, ostrc_2))
  expect_error(find_inj_substantial(ostrc_2, ostrc_3))
})

test_that("Will handle OSTRC variables not coded in the classic 0,8,17,25 responses,
          but throw a warning.", {
            ostrc_2 = c(0, 1, 2, 3)
            ostrc_3 = c(1, 2, 3, 4)
            correct_result1 = c(0, 0, 1, 1)

            ostrc_4 = c(0, 0, 3, 3)
            ostrc_5 = c(3, 3, 0, 0)

            expect_equal(suppressWarnings(find_inj_substantial(ostrc_2, ostrc_3)), correct_result1)
            expect_error(find_inj_substantial(ostrc_4, ostrc_5))
            expect_warning(find_inj_substantial(ostrc_2, ostrc_3))
})
