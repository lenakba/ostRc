context("Calculate mean prevalence for all given hp_types and groups")
library(ostrc)
library(tidyr)
library(dplyr)
library(magrittr)
library(testthat)

# example data used in more than one test
d_ostrc = tribble(~id_participant, ~day_nr, ~hp,
                  1, 1, 1,
                  1, 1, 1,
                  1, 2, 0,
                  2, 1, 1,
                  2, 2, 1,
                  3, 1, 0,
                  3, 2, 0
                  )
