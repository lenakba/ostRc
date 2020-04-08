library(dplyr)

test_that("Calculations are done on correct windows", {
  d_loads = tribble(~load, ~match_cycle,
                    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 360, 1, 100, 1, 540, 2,
                    0, 3, 0, 3, 1080, 3, 630, 3, 540, 3, 810, 3, 180, 3, 0, 3, 180, 3, 480, 3,  540, 4, 540, 5,
                    0, 6, 0, 6, 0, 6, 0, 6, 0, 6, 0, 6)
  d_loads_window1 = c(0, 0, 0, 0, 0, 0, 0, 360, 100, 540, 0, 0, 1080, 630, 540, 810, 180, 0, 180, 480)
  d_loads_window2 = c(540, 0, 0, 1080, 630, 540, 810, 180, 0, 180, 480, 540)
  d_loads_window3 = c(0, 0, 1080, 630, 540, 810, 180, 0, 180, 480, 540, 540)
  load_window1 = ewma(d_loads_window1, n_days = length(d_loads_window1), window = 1) %>% last()
  load_window2 = ewma(d_loads_window2, n_days = length(d_loads_window2), window = 1) %>% last()
  load_window3 = ewma(d_loads_window3, n_days = length(d_loads_window3), window = 1) %>% last()

  expect_equal((micro_cycle_cl(d_loads, load, match_cycle) %>% pull(chronic_load))[1], load_window1)
  expect_equal((micro_cycle_cl(d_loads, load, match_cycle) %>% pull(chronic_load))[2], load_window2)
  expect_equal((micro_cycle_cl(d_loads, load, match_cycle) %>% pull(chronic_load))[3], load_window3)
})

test_that("Missing values are removed before calculation.", {
  d_loads = tribble(~load, ~match_cycle,
                    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 360, 1, 100, 1, 540, 2,
                    0, 3, 0, 3, 1080, 3, 630, 3, 540, 3, 810, 3, 180, 3, 0, 3, 180, 3, 480, 3,  540, 4, 540, 5,
                    0, 6, 0, 6, 0, 6, 0, 6, 0, 6, 0, 6)

  d_loads_missing = tribble(~load, ~match_cycle,
                            0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 360, 1, NA, 1, NA, 1,
                            NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, 100, 1, 540, 2,
                            0, 3, 0, 3, 1080, 3, 630, 3, 540, 3, 810, 3, 180, 3, 0, 3, 180, 3, 480, 3,  540, 4, 540, 5,
                            0, 6, 0, 6, 0, 6, 0, 6, 0, 6, 0, 6)

  expect_equal(micro_cycle_cl(d_loads, load, match_cycle) %>% pull(chronic_load),
               micro_cycle_cl(d_loads_missing, load, match_cycle) %>% pull(chronic_load))
})

test_that("Windows won't become so short EWMA can't be calculated.", {
  d_loads_short = tribble(~load, ~match_cycle,
                          0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 360, 1, 100, 1, 540, 2,
                          0, 3, 0, 3, 1080, 3, 630, 3, 540, 3, 810, 3, 180, 3, 0, 3, 180, 3, 480, 3,  540, 4, 540, 5)
  d_loads_window1 = c(0, 0, 0, 0, 0, 0, 0, 360, 100, 540, 0, 0, 1080, 630, 540, 810, 180, 0, 180, 480)
  d_loads_window2 = c(540, 0, 0, 1080, 630, 540, 810, 180, 0, 180, 480, 540)
  d_loads_window3 = c(0, 0, 1080, 630, 540, 810, 180, 0, 180, 480, 540, 540)
  load_window1 = ewma(d_loads_window1, n_days = length(d_loads_window1), window = 1) %>% last()
  load_window2 = ewma(d_loads_window2, n_days = length(d_loads_window2), window = 1) %>% last()
  load_window3 = ewma(d_loads_window3, n_days = length(d_loads_window3), window = 1) %>% last()

  expect_equal((micro_cycle_cl(d_loads_short, load, match_cycle) %>% pull(chronic_load))[1], load_window1)
  expect_equal((micro_cycle_cl(d_loads_short, load, match_cycle) %>% pull(chronic_load))[2], load_window2)
  expect_equal((micro_cycle_cl(d_loads_short, load, match_cycle) %>% pull(chronic_load))[3], load_window3)
})


test_that("Too short vectors or indices spurr a warning.", {
  d_loads_short = tribble(~load, ~match_cycle, 0, 1, 3, 2)
  d_loads_short_groups = tribble(~load, ~match_cycle, 0, 1, 3, 2, 4, 3)

  expect_error(micro_cycle_cl(d_loads_short, load, match_cycle),
               paste0("Error: Load vector length too short for sliding windows. Length = ",length(d_loads_short$load),"."))
  expect_warning(micro_cycle_cl(d_loads_short_groups, load, match_cycle),
                 "All cycles have only 1 element.")
})
