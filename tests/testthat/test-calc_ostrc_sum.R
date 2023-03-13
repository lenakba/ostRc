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
