context("Calculate prevalence")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

# example vectors used in more than one test
q1 = c(17, 8, 8, 8, 0)
q2_v2 = c(25, 17, 17, 0)
q3_v2 = c(25, 8, 17, 0)
q4 = c(25, 8, 0, 0)

test_that("Returns a vector with correct number of sumscores.", {
  length_q1 = length(q1)
  sumscores_test = calc_ostrc_sum(q1, q2_v2, q3_v2, q4)
  expect_length(sumscores_test, length_q1)
})

test_that("Sumscores are correct.", {
  sumscores_test = calc_ostrc_sum(q1, q2_v2, q3_v2, q4)
  expect_equal(sumscores_test, length_q1)
})

test_that("Returns a value of NA if one ore more responses are missing.", {
  sumscores_test = calc_ostrc_sum(q1, q2_v2, q3_v2, q4)
  expect_equal(sumscores_test, length_q1)
})
