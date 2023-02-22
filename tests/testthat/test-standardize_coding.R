context("Standardize coding function")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

test_that("Converts lowest number to 0, medium number to 8, higher to 17, and highest to 25.", {
  ostrc_q = c(0, 1, 2, 3, 2, 2, 3)
  correct_result = c(0, 8, 17, 25, 17, 17, 25)

  ostrc_q2 = c(1, 2, 4, 3, 2, 1)
  correct_result2 = c(0, 8, 25, 17, 8, 0)

  expect_equal(standardize_coding(ostrc_q), correct_result)
  expect_equal(standardize_coding(ostrc_q2), correct_result2)
})

test_that("Throws error if vector of OSTRC values is non-numeric.", {
  ostrc_q1 = c(TRUE, FALSE, FALSE, TRUE)
  ostrc_q2 = c("Reduced participation", "Feeling pain",
               "Reduced participation", "Feeling great")
  expect_error(standardize_coding(ostrc_q1))
  expect_error(standardize_coding(ostrc_q2))
})

test_that("Throws error if vector of OSTRC values is Q2 or Q3 from OSTRC 1.0.", {
  ostrc_q = c(0, 13, 17, 19, 25)
  expect_error(standardize_coding(ostrc_q))
})

test_that("Throws error if vector of OSTRC values has more than 4 types of values.", {
  ostrc_q = c(0, 1, 2, 3, 4, 5, 6, 7)
  ostrc_q2 = c(0, 1, 2, 3, 4, 99)
  expect_error(standardize_coding(ostrc_q))
  expect_error(standardize_coding(ostrc_q2))
})

test_that("Throws error if vector of OSTRC values is already coded with the standard 0, 8, 17, 25 codes.", {
  ostrc_q = c(0, 8, 17, 25)
  ostrc_q2 = c(17, 25, 17, 25)
  expect_error(standardize_coding(ostrc_q))
  expect_error(standardize_coding(ostrc_q2))
})

test_that("Will return missing values (NA) as missing values (NA).", {
  ostrc_q = c(0, 1, 2, NA)
  correct_result = c(0, 8, 17, NA)
  expect_equal(standardize_coding(ostrc_q), correct_result)
})

test_that("Returns error if there are no fewer than 4 codes in the input vector.", {
  ostrc_q = c(0, 3, 3, 0)
  ostrc_q2 = c(0, 0, 0, 0)

  expect_error(standardize_coding(ostrc_q))
  expect_error(standardize_coding(ostrc_q2))
})
