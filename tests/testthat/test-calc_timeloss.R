context("Calculate time loss from OSTRC questionnaires.")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)
library(nplyr)

d_ostrc = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1,
                  1, 1, "2023-01-01", 8,
                  1, 1, "2023-01-07", 8,
                  1, 1, "2023-01-14", 8,
                  1, 1, "2023-01-21", 25,
                  1, 18, "2022-12-07", 25,
                  1, 18, "2022-12-14", 25,
                  2, 2, "2023-01-12", 8,
                  3, NA, "2022-06-05", 0)

test_that("Returns a numeric vector.",
          {
            timeloss_vector = calc_timeloss(d_ostrc, id_case, date_ostrc, q1)
            expect_vector(timeloss_vector, ptype = numeric())
          })

test_that("Returns a numeric vector of length equal to the number of id_cases.",
          {
            n_cases = length(unique(na.omit(d_ostrc$id_case)))
            timeloss_vector = calc_timeloss(d_ostrc, id_case, date_ostrc, q1)
            expect_equal(length(timeloss_vector), n_cases)
          })

test_that("Results with OSTRC1 = 0 are ignored and not included in final vector.",
          {
            d_lotsof0s = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1,
                              1, NA, "2023-01-01", 0,
                              1, NA, "2023-01-07", 0,
                              1, NA, "2023-01-14", 0,
                              1, NA, "2023-01-21", 0,
                              1, 18, "2022-12-07", 25,
                              1, 18, "2022-12-14", 25,
                              2, 2, "2023-01-12", 8,
                              3, NA, "2022-06-05", 0)
            length_vector = 2
            timeloss_vector = calc_timeloss(d_lotsof0s, id_case, date_ostrc, q1)
            expect_equal(length(timeloss_vector), length_vector)
          })

test_that("If there are any responses of OSTRC1 = 0, but they still have a case_id, will get a warning.",
          {
            d_ostrc_nonmissing_and0 = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1,
                              1, NA, "2023-01-01", 0,
                              1, NA, "2023-01-07", 0,
                              1, NA, "2023-01-14", 0,
                              1, NA, "2023-01-21", 0,
                              1, 18, "2022-12-07", 25,
                              1, 18, "2022-12-14", 25,
                              2, 2, "2023-01-12", 8,
                              3, 3, "2022-06-05", 0)
            expect_warning(calc_timeloss(d_ostrc_nonmissing_and0, id_case, date_ostrc, q1))
          })

test_that("Returns timeloss of 1 week if health problem
          started and ended on one week, with OSTRC value 25.",
          {
            correct_timeloss = c(1, 2, 0) # based on original d_ostrc example
            est_timeloss = calc_timeloss(d_ostrc, id_participant,
                                         id_case, date_ostrc,
                                         q1)
            expect_equal(est_timeloss, correct_timeloss)
          })

test_that("Throws error if a health problem does not have a case id.",
          {
            d_missing_caseid = d_ostrc
            d_missing_caseid[2,2] = NA

            expect_error(calc_timeloss(d_missing_caseid, id_participant,
                                          id_case, date_ostrc,
                                          q1))
          })

test_that("Returns error if OSTRC 1 is non-numeric.",
          {
            d_test = d_ostrc %>% mutate(q1 = as.character(q1))
            expect_error(calc_timeloss(d_test, id_participant,
                                          id_case, date_ostrc,
                                          q1))
          })
