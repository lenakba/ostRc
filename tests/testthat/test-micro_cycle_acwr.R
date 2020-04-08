library(dplyr)

test_that("Acute loads are matched with correct chronic loads.", {
  d_acute_chronic = tribble(~acute_load, ~chronic_load,
                            51.1, 228,
                            540, 424,
                            390, 369,
                            540, 473,
                            540, 496)

  acwr_chronic3 = c(390/228, 540/424, 540/369)
  acwr_chronic4 = c(540/228, 540/424)

  expect_equal((micro_cycle_acwr(d_acute_chronic, 2) %>% pull(acwr))[1], acwr_chronic3[1])
  expect_equal((micro_cycle_acwr(d_acute_chronic, 2) %>% pull(acwr))[2], acwr_chronic3[2])
  expect_equal((micro_cycle_acwr(d_acute_chronic, 2) %>% pull(acwr))[3], acwr_chronic3[3])

  expect_equal((micro_cycle_acwr(d_acute_chronic, 3) %>% pull(acwr))[1], acwr_chronic4[1])
  expect_equal((micro_cycle_acwr(d_acute_chronic, 3) %>% pull(acwr))[2], acwr_chronic4[2])
})
