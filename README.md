Oslo Sports Trauma Research Centre R Tools (OSTRC package)
================
Lena Kristin Bache-Mathiesen[^1]Benjamin Clarsen[^2]

## Overview

The OSTRC package is a collection of R functions for working with sports
injury data. Among other functions, the package provides tools for
handling data from the Oslo Sports Trauma Research Centre Questionnaire
on overuse injuries or health problems.

### OSTRC questionnaire

The OSTRC questionnaire collects self-reported injury and illness
data.  
The functions for handling OSTRC-questionnaire data are generalized to
work with any OSTRC-questionnaire type and version. The overuse
questionnaire is denoted OSTRC-O. The health problem questionnaire is
denoted OSTRC-H. Version 1.0 corresponds to the original introduction of
the questionnaires, version 2.0 to the updated version after the review
panel meeting. Based on the articles they were introduced, in
chronological order from the earliest to latest version:

<ol>
<li>
Original overuse questionnaire: [OSTRC-O
1.0](doi.org/10.1136/bjsports-2012-091524)
</li>
<li>
The expanded version capturing all health problems: [OSTRC-H
1.0](doi.org/10.1136/bjsports-2012-092087)
</li>
<li>
The review panel meeting updating both questionnaires: [OSTRC-O 2.0 and
OSTRC-H 2.0](doi.org/10.1136/bjsports-2019-101337)
</li>
</ol>

## Installation

``` r
# Requires the devtools package
# If devtools is not installed, run code:
# install.packages("devtools") before installing ostrc
devtools::install_github("lenakba/ostrc")
```

## Functions

Below is a brief overview of helpful functions.

### Create case data

The function `create_case_data` finds health problems in a dataset with
OSTRC-questionnaire responses and returns a dataframe where one row
describes one unique health problem. The function also finds and adds
the startdate, enddate, and the duration (in days) of each health
problem. It also identifies substantial health problems, with the help
of `find_hp_substantial`, and adds a column for these.

``` r
library(tidyverse)
d_ostrc = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4,
                   1, 1, "2023-01-01", 0, 0, 17, 25,
                   1, 1, "2023-01-07", 8, 0, 17, 25,
                   1, 1, "2023-01-19", 8, 0, 17, 0,
                   1, 18, "2022-12-07", 25, 0, 0, 0,
                   2, 2, "2023-01-12", 8, 8, NA, NA,
                   3, 3, "2022-06-05", 0, 0, 0, 0)
# note that the date column must be of class date
d_ostrc = d_ostrc %>% mutate(date_ostrc = as.Date(date_ostrc))

# functions returns one row per health problem
# if a response is not a health problem (like participant ID 3, case ID 3)
# it will not be included in the returned data frame
create_case_data(d_ostrc, id_participant, id_case, date_ostrc, q1, q2, q3, q4)
```

    ## # A tibble: 3 × 11
    ##   id_case id_part…¹ date_start date_end   durat…² hp_sub    q1    q2    q3    q4
    ##     <dbl>     <dbl> <date>     <date>       <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1       1         1 2023-01-07 2023-01-19      13      1     8     0    17    25
    ## 2      18         1 2022-12-07 2022-12-07       1      1    25     0     0     0
    ## 3       2         2 2023-01-12 2023-01-12       1      0     8     8    NA    NA
    ## # … with 1 more variable: date_ostrc <date>, and abbreviated variable names
    ## #   ¹​id_participant, ²​duration
    ## # ℹ Use `colnames()` to see all variable names

### Find health problems and substantial health problems

Given a vector of OSTRC responses to question 1, The function `find_hp`
identifies whether or not an observation is a health problem as per the
definition of a health problem in the OSTRC questionnaire.

``` r
ostrc_1 = c(0, 8, 17, 25)

find_hp(ostrc_1)
```

    ## [1] 0 1 1 1

If the vector of responses to question 1 is not coded with the standard
0, 8, 17, 25 responses, but with 1 to 4, or 0 to 3, these can also work.
In these cases, `find_hp` uses the `standardize_codes` function
automatically.

``` r
ostrc_1_other_values = c(0, 1, 2, 3)

find_hp(ostrc_1_other_values)
```

    ## [1] 0 1 1 1

Substantial health problems are more complicated to identify, and
depends on the OSTRC version (1.0 or 2.0). The function
`find_hp_substantial` identifies substantial injuries given question 1,
2, and 3 of the OSTRC questionnaire.

``` r
ostrc_1 = c(0, 8, 8, 8)
ostrc_2 = c(0, 0, 0, 25)
ostrc_3 = c(0, 0, 17, 0)
   
find_hp_substantial(ostrc_1, ostrc_2, ostrc_3)
```

    ## [1] NA  0  1  1

Make note of how `find_hp_substantial` treats missing in one or more of
the OSTRC-questionnaire items. If one of the three questions indicate a
substantial health problem, `find_hp_substantial` will consider it
substantial, regardless of missing values in other items. No matter
whether the missing values are, in reality, substantial or
non-substantial, we still have enough information to conclude that the
observation is substantial.

However, if the only non-missing responses are values that do not
indicate a substantial health problem, then it will return as missing.
In these cases, there is no way of knowing whether one of the missing
observations was, in reality, a response pertaining to a substantial
health problem.

``` r
ostrc_1_missing = c(25, NA, NA, NA)
ostrc_2_missing = c(NA, NA, NA, 17)
ostrc_3_missing = c(NA, 8, NA, NA)

find_hp_substantial(ostrc_1_missing, ostrc_2_missing, ostrc_3_missing)
```

    ## [1]  1 NA NA  1

### Find and add event IDs

Function `add_event_id` finds intervals for each event in a longitudinal
dataset, and adds a column indexing them. The intervals may be of
unequal lengths. This is useful for finding intervals between multiple
injuries or episodes of pain on the same individual, that is,
identifying injury cases. Or the opposite: the intervals where the
participant is currently injured or in pain.

It can also be used to find the so-called micro-cycle between matches
and competition events. Micro-cycles are the time from a previous
competition or match to the next competition or match. That is, all days
up to first match/competition in one cycle, all days from first match to
second match in the next cycle and so on. Coaches often plan training
according to such micro-cycles.

This is handy for counting the number of injury cases or number of
micro-cycles, calculating the average interval time, or calculating the
average of anything else per injury or other event.

``` r
library(tidyverse)
library(ostrc)
d_matches_sim = tribble(~date, ~match,
                          "2017-08-22", 0,
                          "2017-08-23", 1,
                          "2017-08-24", 0,
                          "2017-08-25", 0,
                          "2017-08-26", 1)
  d_matches_sim = d_matches_sim %>% mutate(date = lubridate::as_date(date))
  add_event_id(d_matches_sim, date, match)
```

    ## # A tibble: 5 × 3
    ##   date       match event_id
    ##   <date>     <dbl> <chr>   
    ## 1 2017-08-22     0 1       
    ## 2 2017-08-23     1 1       
    ## 3 2017-08-24     0 2       
    ## 4 2017-08-25     0 2       
    ## 5 2017-08-26     1 2

## Acknowledgments

Thank you to Prof. Roald Bahr for prioritizing this project. Thanks also
to the Oslo Sports Trauma Research Centre for providing a supportive
working environment.

## Author Contributions

Author BC wrote the original code for handling the OSTRC-questionnaire
and calculating incidences and prevalences. Author LKBM generalized BC’s
code and developed the R package.

[^1]: Oslo Sports Trauma Research Centre, <lenakba22@gmail.com>

[^2]: Oslo Sports Trauma Research Centre, <benjaminc@nih.no>
