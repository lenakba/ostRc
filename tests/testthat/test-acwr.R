#------------------------------------------------------Testing
# The tests below are to make sure the acwr-function works correctly.
# Cheers! #test-driven-development
# https://en.wikipedia.org/wiki/Test-driven_development

context("ACWR function")
library(ostrc)
library(magrittr)

# Test that acwr() informs the user of missing values
test_that("Gives warning at missing values for RA, and error for EWMA", {
  tl_miss = sample(500:1300, 28, replace = TRUE) %>% dplyr::replace(., 9:12, NA)
  expect_warning(acwr(tl_miss), "Ignoring 4 missing values.")
})

# acwr gives an error when the user runs it on a vector with non-leading NA
test_that("Leading NA-values should not stop EWMA calculation, but non-leading does and gives error message", {
  tl_miss = sample(500:1300, 28, replace = TRUE) %>% dplyr::replace(., 9:12, NA)
  tl_miss_leading = sample(500:1300, 28, replace = TRUE) %>% dplyr::replace(., 1:3, NA)
  expect_equal(length(acwr(tl_miss_leading, fun = "ewma")), length(tl_miss_leading))
  expect_error(acwr(tl_miss, fun = "ewma"), "Error: EWMA cannot be calculated on vectors with non-leading NA elements.")
})

# Test that acwr() gives a warning message about vectors shorter than acute length
test_that("Gives warning at vectors shorter than acute day value", {
  tl_short = 1000:1003
  expect_warning(acwr(tl_short), "Object length is less than acute period. Length = 4, Acute = 7")
})

# Test that the acwr-function returns a vector of correct length and type
test_that("Returns correct length, element and type.", {
  tl = sample(500:1300, 28, replace = TRUE)
  expect_length(acwr(tl), length(tl))
  expect_length(acwr(tl, fun = "ewma"), length(tl))
  expect_type(acwr(tl), "double")
})

# Test that the function returns an error at incorrect data types
test_that("Returns error message at incorrect data types", {
  tl_text = c("a little", "some", "a lot")
  expect_error(acwr(tl_text))
})

# test that the rolling averages return missing values for n values after a missing value
# where n is the number of days chosen - 1
# This was mostly for curiosty's sake,
# as the rolling average function is based off of rollapplyr, not built from scratch
test_that("Calculation of Rolling averages does not use values immediately after NA for claculation", {
  tl_miss = sample(500:1300, 28, replace = TRUE) %>% dplyr::replace(., 9:12, NA)
  last_missing = max(which(is.na((tl_miss))))
  seq_missing = seq(last_missing, last_missing + 6)
  seq_func_missing = which(is.na(ra(tl_miss, 7)))
  expect_true(all(seq_missing %in% seq_func_missing))
})

# Testing that the uncoupled-equation does indeed give the same results as doing uncoupled calculation manually
test_that("Running equation on coupled ACWR to recieve uncoupled ACWR gives the same result as calculating uncoupled ACWR manually", {
  tl_acute = sample(500:1300, 7, replace = TRUE)
  tl_chronic= c(tl_acute, sample(500:1300, 21, replace = TRUE))

  ra_acute = ra(tl_acute, 7, window = 7)
  ra_chronic_coupled = ra(tl_chronic, 28, window = 28)
  ra_chronic_uncoupled = ra(tl_chronic[8:28], 21, window = 21)

  acwr_coupled = ra_acute/ra_chronic_coupled
  acwr_uncoupled = ra_acute/ra_chronic_uncoupled # calculating manually
  acwr_uncoupled_eq = (3 * acwr_coupled)/(4 - acwr_coupled) # using equation on coupled version

  expect_equal(acwr_uncoupled, acwr_uncoupled_eq)
})
