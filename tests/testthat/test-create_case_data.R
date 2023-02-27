context("Make health problem caselist function")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

d_ostrc = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4,
                  1, 1, "2023-01-01", 0, 0, 17, 25,
                  1, 1, "2023-01-07", 8, 0, 17, 25,
                  1, 1, "2023-01-19", 8, 0, 17, 0,
                  1, 18, "2022-12-07", 25, 0, 0, 0,
                  2, 2, "2023-01-12", 8, 8, NA, NA,
                  3, 3, "2022-06-05", 0, 0, 0, 0)

d_ostrc = d_ostrc %>% mutate(date_ostrc = as.Date(date_ostrc))

test_that("All case IDs are included in the new dataframe,
          if they are health problems.",
          {
            d_test = create_case_data(
              d_ostrc, id_participant, id_case, date_ostrc,
              q1, q2, q3, q4)
            pos_ids = which(find_hp(d_ostrc$q1) == 1)
            d_participants_with_cases = d_ostrc[pos_ids,]

            expect_true(all(d_participants_with_cases$id_case %in%
                              d_test$id_case))
          })

test_that("Non-healthproblems are not included in the final dataframe.",
          {
            d_test = create_case_data(
              d_ostrc, id_participant, id_case, date_ostrc,
              q1, q2, q3, q4)
            expect_true(all(d_test$q1 > 0))
          })

test_that("Returns a dataset with output columns
          named the same as the input columns.",
          {
           d_othernames = d_ostrc %>%
                          rename(p_id = id_participant, injury_id = id_case)
           creatednames = names(create_case_data(
                                  d_othernames, p_id, injury_id, date_ostrc,
                                  q1, q2, q3, q4))
           expect_true(all(names(d_othernames) %in% creatednames))
          })

test_that("Returns correct dates.",
          {
            correct_startdate = as.Date(c("2023-01-07", "2022-12-07",
                                          "2023-01-12"))

            correct_enddate = as.Date(c("2023-01-19", "2022-12-07",
                                          "2023-01-12"))

            d_created = create_case_data(d_ostrc, id_participant,
                                         id_case, date_ostrc,
                                         q1, q2, q3, q4)
            expect_equal(d_created$date_start, correct_startdate)
            expect_equal(d_created$date_end, correct_enddate)
          })

test_that("Returns duration of 1 day if health problem
          started and ended on same day.",
          {
            correct_duration = c(13, 1, 1) # note that the current day is counted as 1
            d_created = create_case_data(d_ostrc, id_participant,
                                         id_case, date_ostrc,
                                         q1, q2, q3, q4)
            expect_equal(d_created$duration, correct_duration)
          })

test_that("There are no duplicates.",
          {
            d_created = create_case_data(d_ostrc, id_participant,
                                         id_case, date_ostrc,
                                         q1, q2, q3, q4)
            expect_false(all(duplicated(d_created)))
          })

test_that("Adds extra variables if there
          were extra in the original data.",
          {
            d_expanded = cbind(d_ostrc,
                               comment = c("Painful",
                                           "Just above the knee",
                                           "Gradual onset",
                                           "Mondays", "", "Doctor not present"))
            d_created = create_case_data(d_expanded, id_participant,
                                         id_case, date_ostrc,
                                         q1, q2, q3, q4)

            expect_true("comment" %in% names(d_created))
          })

test_that("Throws error if a health problem does not have a case id.",
          {
            d_missing_caseid = d_ostrc
            d_missing_caseid[2,2] = NA

            expect_error(create_case_data(d_missing_caseid, id_participant,
                                         id_case, date_ostrc,
                                         q1, q2, q3, q4))
          })
