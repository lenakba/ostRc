context("Find injuries function")
library(ostRc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

test_that("Returns 0 if Q1 equals 0, returns 1 if Q1 does not equal 0.", {
            ostrc_1 = c(0, 8, 25, 0)
            correct_result = c(0, 1, 1, 0)
            expect_equal(find_hp(ostrc_1), correct_result)
})

test_that("Returns NA if input value is NA.", {
  ostrc_1 = c(0, 8, 25, NA)
  correct_result = c(0, 1, 1, NA)
  expect_equal(find_hp(ostrc_1), correct_result)
})

test_that("Returns error if the input vector is non-numeric.", {
  ostrc_1 = c("foo", "bar")
  ostrc_1 = c(TRUE, FALSE)
  expect_error(find_hp(ostrc_1))
  expect_error(find_hp(ostrc_2))
})

test_that("Returns warning if there are no health problems, also if some ar missing.", {
  ostrc_1 = c(0, 0, 0)
  ostrc_2 = c(0, 0, NA)
  expect_warning(find_hp(ostrc_1))
  expect_warning(find_hp(ostrc_2))
})

test_that("Will handle OSTRC vectors not coded in the classic 0,8,17,25 responses,
          but throw a warning.", {
            ostrc_1 = c(0, 1, 2, 3)
            correct_result1 = c(0, 1, 1, 1)
            expect_warning(find_hp(ostrc_1))
            expect_equal(suppressWarnings(find_hp(ostrc_1)), correct_result1)

            ostrc_2 = c(0, 0, 3, 3)
            expect_error(find_hp(ostrc_2))
          })
