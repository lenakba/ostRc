# Testing the function for smart rounding
library(ostrc)

test_that("Rounding with smart_round() should be equivalent to preferred choices in rounding manually with round()", {
  a = c(1.567358, 1.99993, 1.3352, 1.57333)
  b = c(1.567358, 3.4678368, 10.2645747)
  c = c(1.567358, 1.54000, 1.53803)
  expect_equal(round(a, 1), smart_round(a))
  expect_equal(round(b), smart_round(b))
  expect_equal(round(c, 2), smart_round(c))
})

test_that("Missing values are skipped, but included in returned vector.", {
  a = c(1.567358, NA, 1.3352, 1.57333)
  expect_equal(round(a, 1), smart_round(a))
})
