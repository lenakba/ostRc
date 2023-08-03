context("Make health problem caselist function")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)
library(nplyr)

d_ostrc = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4,
                  1, 1, "2023-01-01", 8, 0, 17, 25,
                  1, 1, "2023-01-07", 8, 0, 17, 25,
                  1, 1, "2023-01-14", 8, 0, 17, 0,
                  1, 18, "2022-12-07", 25, 0, 0, 0,
                  2, 2, "2023-01-12", 8, 8, 0, 0,
                  3, NA, "2022-06-05", 0, 0, 0, 0)

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
            expect_true(all(d_test$id_participant < 3))
          })

test_that("Returns a dataset with output columns
          named the same as the input columns.",
          {
           othernames = c("p_id", "injury_id")
           creatednames = names(create_case_data(
                                  d_othernames, p_id, injury_id, date_ostrc,
                                  q1, q2, q3, q4))
           expect_true(all(othernames %in% creatednames))
          })

test_that("Returns correct dates.",
          {
            correct_startdate = as.Date(c("2023-01-01", "2022-12-07",
                                          "2023-01-12"))

            correct_enddate = as.Date(c("2023-01-14", "2022-12-07",
                                          "2023-01-12"))

            d_created = create_case_data(d_ostrc, id_participant,
                                         id_case, date_ostrc,
                                         q1, q2, q3, q4)
            expect_equal(d_created$date_start, correct_startdate)
            expect_equal(d_created$date_end, correct_enddate)
          })

test_that("Returns duration of 1 week if health problem
          started and ended on one week.",
          {
            correct_duration = c(3, 1, 1) # note that the current week is counted as 1
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

test_that("Gives correct substantial health problems
          if older version of OSTRC is used.",
          {
            d_1_0 = d_ostrc %>% mutate(q1 = c(17,
                                              17,
                                              17,
                                              0, 8, 0),
                                       q2 = c(13,
                                              13,
                                              13,
                                              0, 0, 0),
                                       q3 = c(17,
                                           17,
                                           17,
                                           0, 0, 0))
            correct_hp_sub = c(1, 0)

            d_created_oldversion = suppressWarnings(create_case_data(d_1_0, id_participant,
                                          id_case, date_ostrc,
                                          q1, q2, q3, q4, version = "1.0"))
            expect_equal(d_created_oldversion$hp_sub, correct_hp_sub)
          })

test_that("Returns error if OSTRC 1 is non-numeric.",
          {
            d_test = d_ostrc %>% mutate(q1 = as.character(q1))
            expect_error(create_case_data(d_test, id_participant,
                                         id_case, date_ostrc,
                                         q1, q2, q3, q4))
          })

test_that("will return dataframe without
           substantial health problem column
           if find_substantial throws error.",
          {
            d_test = d_ostrc %>% mutate(q3 = c(NA,
                                              NA,
                                              NA,
                                              NA, NA, NA),
                                       q3 = c(NA,
                                              NA,
                                              NA,
                                              NA, NA, NA))

            expect_warning(create_case_data(d_test, id_participant,
                                            id_case, date_ostrc,
                                            q1, q2, q3, q4))

            d_created = suppressWarnings(
              create_case_data(d_test, id_participant,
                             id_case, date_ostrc,
                             q1, q2, q3, q4))

            expect_true(all(names(d_created) != "hp_sub"))
          })

test_that("Returns warning if there are any duplicates in the original data.",
          {
            d_duplicated = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4,
                              1, 1, "2023-01-01", 8, 0, 17, 25,
                              1, 1, "2023-01-07", 8, 0, 17, 25,
                              1, 1, "2023-01-07", 8, 0, 17, 25)
            d_duplicated = d_duplicated %>% mutate(date_ostrc = as.Date(date_ostrc))

            expect_warning(create_case_data(d_duplicated, id_participant,
                                          id_case, date_ostrc,
                                          q1, q2, q3, q4))
          })

test_that("Returns warning if the first OSTRC Q has missing data.",
          {
            d_missing_q1 = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4,
                                   1, 1, "2023-01-01", NA, 0, 17, 25,
                                   1, 1, "2023-01-07", 8, 0, 17, 25)
            expect_warning(create_case_data(d_missing_q1, id_participant,
                                            id_case, date_ostrc,
                                            q1, q2, q3, q4))
          })

test_that("Returns warning if id_participant has missing data.",
          {
            d_missing_q1 = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4,
                                   NA, 1, "2023-01-01", 8, 0, 17, 25,
                                   1, 1, "2023-01-07", 8, 0, 17, 25)
            expect_warning(create_case_data(d_missing_q1, id_participant,
                                            id_case, date_ostrc,
                                            q1, q2, q3, q4))
          })

test_that("Returns warning if id_participant has missing data.",
          {
            d_missing_q1 = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4,
                                   1, 1, NA, 8, 0, 17, 25,
                                   1, 1, "2023-01-07", 8, 0, 17, 25)
            expect_warning(create_case_data(d_missing_q1, id_participant,
                                            id_case, date_ostrc,
                                            q1, q2, q3, q4))
          })

test_that("Adds a column for severity scores.",
          {
            d_test = create_case_data(d_ostrc, id_participant,
                                      id_case, date_ostrc,
                                      q1, q2, q3, q4)

            expect_true(any(names(d_test) %in% "severity_score"))
            for(i in nrow(d_test)) {
              expect_gte(d_test$severity_score[i], 0)
              expect_lte(d_test$severity_score[i], 100)
            }
          })

test_that("Adds a column for time loss.",
          {d_test = create_case_data(d_ostrc, id_participant,
                                      id_case, date_ostrc,
                                      q1, q2, q3, q4)

            expect_true(any(names(d_test) %in% "timeloss"))
          })
