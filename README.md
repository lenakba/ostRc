Oslo Sports Trauma Research Centre R package
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

Below is a brief tutorial on helpful functions in the R package.

### OSTRC questionnaire functions

#### Find health problems and substantial health problems

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
2, and 3 of the OSTRC questionnaire. If there is no health problem, it
will return as missing.

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

#### Calculate severity score and timeloss

Severity scores can also be calculated with `calc_severity_score`.

``` r
ostrc_1 = c(0, 8, 8, 8)
ostrc_2 = c(0, 0, 0, 25)
ostrc_3 = c(0, 0, 17, 0)
ostrc_4 = c(0, 0, 25, 25)
   
calc_severity_score(ostrc_1, ostrc_2, ostrc_3, ostrc_4)
```

    ## [1]  0  8 50 58

Finally, one can determine the timeloss of each health problem with
`calc_timeloss`. You receive a vector with the number of weeks lost per
health problem case in the data. In the example below, there are 3 case
IDs representing 3 health problem cases, and we receive 3 values
representing the number of weeks lost.

``` r
d_ostrc = tribble(~id_participant, ~id_case, ~date_sent, ~q1,
                  1, 1, "2023-01-01", 8,
                  1, 1, "2023-01-07", 8,
                  1, 1, "2023-01-14", 8,
                  1, 1, "2023-01-21", 25,
                  1, 18, "2022-12-07", 25,
                  1, 18, "2022-12-14", 25,
                  2, 2, "2023-01-12", 8,
                  3, NA, "2022-06-05", 0)
   
calc_timeloss(d_ostrc, id_participant, id_case, date_sent, q1)
```

    ## [1] 1 2 0

#### Create case data

The function `create_case_data` is a helper function that brings all the
information you need on each health problem in one dataset. It finds
health problems, substantial health problems, severity score, startdate,
enddate, and timeloss and duration (in weeks). Returns a dataframe where
one row describes one unique health problem. Any other information on
the health problem (type, location etc.) is retained.

``` r
library(tidyverse) # for tribble() and pipe %>% 
d_ostrc = tribble(~id_participant, ~id_case, ~date_ostrc, ~q1, ~q2, ~q3, ~q4, ~hb_type, ~inj_type,
                  1, 1, "2023-01-01", 8, 0, 17, 25, "Injury", "Overuse",
                  1, 1, "2023-01-07", 8, 0, 17, 25, "Injury", "Overuse",
                  1, 1, "2023-01-14", 8, 0, 17, 0, "Injury", "Overuse",
                  1, 18, "2022-12-07", 25, 0, 0, 0, "Illness", NA,
                  2, 2, "2023-01-12", 8, 8, 0, 0, NA, NA,
                  3, 3, "2022-06-05", 0, 0, 0, 0, NA, NA,
                  4, 4, "2023-01-01", 8, 8, 8, 0, "Injury", "Acute")

# note that the date column must be of class date
d_ostrc = d_ostrc %>% mutate(date_ostrc = as.Date(date_ostrc))

# functions returns one row per health problem
# if a response is not a health problem (like participant ID 3, case ID 3)
# it will not be included in the returned data frame.
d_cases = create_case_data(d_ostrc, id_participant, id_case, date_ostrc, q1, q2, q3, q4)
d_cases
```

    ## # A tibble: 4 × 10
    ##   id_case id_part…¹ date_start date_end   durat…² timel…³ hp_sub sever…⁴ hb_type
    ##     <dbl>     <dbl> <date>     <date>       <dbl>   <dbl>  <dbl>   <dbl> <chr>  
    ## 1       1         1 2023-01-01 2023-01-14       3       0      1      50 Injury 
    ## 2      18         1 2022-12-07 2022-12-07       1       1      1      25 Illness
    ## 3       2         2 2023-01-12 2023-01-12       1       0      0      16 <NA>   
    ## 4       4         4 2023-01-01 2023-01-01       1       0      0      24 Injury 
    ## # … with 1 more variable: inj_type <chr>, and abbreviated variable names
    ## #   ¹​id_participant, ²​duration, ³​timeloss, ⁴​severity_score
    ## # ℹ Use `colnames()` to see all variable names

Any extra columns in the dataset will be included at the end, like
columns `hb_type` and `inj_typ` in the example above. Below, we show how
these columns appear after being handled by the `create_case_data`
function.

``` r
d_cases %>% select(id_participant, id_case, hb_type, inj_type)
```

    ## # A tibble: 4 × 4
    ##   id_participant id_case hb_type inj_type
    ##            <dbl>   <dbl> <chr>   <chr>   
    ## 1              1       1 Injury  Overuse 
    ## 2              1      18 Illness <NA>    
    ## 3              2       2 <NA>    <NA>    
    ## 4              4       4 Injury  Acute

### Functions for handling injury data in general

#### Calculate prevalence

The OSTRC package has three functions for calculating the prevalence.
`calc_prevalence` calculates the prevalence per given timepoint. The
timepoint can be given by dates, or numeric values, such as study week
number. The example in this readme is an OSTRC-questionnaire sent
weekly. `calc_prevalence_mean` calculates the mean prevalence, after
calculating the weekly prevalence with `calc_prevalence`. Meaning in the
exmaple below, we will obtain the mean weekly prevalence. Finally,
`calc_prevalence_all` calculates the prevalence of each given health
problem type. This is practical in cases where you have multiple types
of health problems, such as illnesses, injuries, contact injuries, acute
injuries etc. and you wish to calculate the prevalence for each type.

``` r
# Here we have some example data
# note that we assume the date the questionnaire was sent
# was the day that respondents replied
d_injuries = tribble(~id_participant, ~date_sent, ~injury, ~injury_substantial, ~gender,
                  1, "2023-01-07", 1, 0, "Male",
                  1, "2023-01-14", 1, 1, "Male",
                  1, "2023-01-21", 0, 0, "Male",
                  2, "2023-01-07", 1, 1, "Female",
                  2, "2023-01-14", 0, 1, "Female",
                  2, "2023-01-21", 1, 0, "Female",
                  3, "2023-01-07", 1, 0, "Male",
                  3, "2023-01-14", 0, 0, "Male",
                  4, "2023-01-07", 0, 0, "Male",
                  4, "2023-01-14", 1, 0, "Male",
                  4, "2023-01-21", 1, 0, "Male",
                  5, "2023-01-07", 1, 0, "Female",
                  5, "2023-01-14", 1, 0, "Female",
                  6, "2023-01-07", 1, 1, "Female",
                  6, "2023-01-14", 1, 1, "Female",
                  6, "2023-01-21", 1, 0, "Female",
                  )
```

Below, we calculate the weekly injury prevalence on the example data,
with the help of `calc_prevalence`. We need to provide the dataframe,
the column with participant or player ID, the date or time the OSTRC
questionnaire was sent, and the health problem column.

``` r
calc_prevalence(d_injuries, id_participant, date_sent, injury)
```

    ## # A tibble: 3 × 4
    ##   date_sent  n_responses n_cases prev_cases
    ##   <chr>            <int>   <dbl>      <dbl>
    ## 1 2023-01-07           6       5      0.833
    ## 2 2023-01-14           6       4      0.667
    ## 3 2023-01-21           4       3      0.75

However, sometimes we are only interested in the mean weekly prevalence.
`calc_prevalence_mean` provides the mean, standard deviation, and
confidence intervals. You can choose confidence interval level, and the
default is at 95%. Note that the function arguments are the same as for
`calc_prevalence`.

``` r
calc_prevalence_mean(d_injuries, id_participant, date_sent, injury)
```

    ## # A tibble: 1 × 4
    ##   prev_mean prev_sd prev_ci_lower prev_ci_upper
    ##       <dbl>   <dbl>         <dbl>         <dbl>
    ## 1      0.75  0.0833         0.543         0.957

When you have multiple health problem classifications, it can be tedious
to calculate the prevalence manually for each one. With
`calc_prevalence_all` you can do them all in one go, with one line of
code. The name of each health problem column needs to be provided as a
vector of strings, like in the example below.

``` r
calc_prevalence_all(d_injuries, id_participant, date_sent, c("injury", "injury_substantial"))
```

    ## # A tibble: 2 × 5
    ##   hp_type            prev_mean prev_sd prev_ci_lower prev_ci_upper
    ##   <chr>                  <dbl>   <dbl>         <dbl>         <dbl>
    ## 1 injury                 0.75   0.0833         0.543         0.957
    ## 2 injury_substantial     0.278  0.255         -0.355         0.910

In some cases, you may wish to calculate prevalences for each category
in a group. For instance, for males and females separately. Below is an
example.

``` r
calc_prevalence_all(d_injuries, id_participant, date_sent, c("injury", "injury_substantial"), "gender")
```

    ## # A tibble: 4 × 6
    ##   gender hp_type            prev_mean prev_sd prev_ci_lower prev_ci_upper
    ##   <chr>  <chr>                  <dbl>   <dbl>         <dbl>         <dbl>
    ## 1 Male   injury                 0.611  0.0962         0.372         0.850
    ## 2 Female injury                 0.889  0.192          0.411         1.37 
    ## 3 Male   injury_substantial     0.111  0.192         -0.367         0.589
    ## 4 Female injury_substantial     0.444  0.385         -0.512         1.40

#### Calculate incidence

Similar to prevalence, calculating incidence also has three functions:
`calc_incidence`, `calc_incidence_mean` and `calc_incidence_all`. The
functions check whether or not the participant had a health problem of
the given type in the previous response. If they did, any response of a
continued health problem will not be considered a new case. If they did
not, a response of a health problem will be considered a new health
problem, and enter the numerator of the incidence calculation. Weekly
incidences can therefore only be calculated if there are no gaps in the
data, i.e. if the questionnaire was sent bi-weekly. If the questionnaire
was sent monthly, this will provide a rough estimate of the monthly
incidence.

Using the same example as the prevalence calculations, here is the
weekly incidence:

``` r
calc_incidence(d_injuries, id_participant, date_sent, injury)
```

    ## # A tibble: 3 × 4
    ##   date_sent  n_responses n_new_cases inc_cases
    ##   <chr>            <int>       <dbl>     <dbl>
    ## 1 2023-01-07           6          NA    NA    
    ## 2 2023-01-14           6           1     0.167
    ## 3 2023-01-21           4           1     0.25

Note that the first row of incidence is missing data. This is because we
do not have health problem information about any of the individuals
before the study started. We therefore cannot know if a health problem
response is a new or continued health problem. The only exception is if
all of the responses are 0 for no health problem, then we can know for
sure that the incidence is 0%. Below is an example.

``` r
 d_0atstart = tribble(~id_participant, ~week_nr, ~hp,
                              1, 1, 0,
                              1, 2, 0,
                              1, 3, 1,
                              2, 1, 0,
                              2, 2, 0,
                              2, 3, 1)
calc_incidence(d_0atstart, id_participant, week_nr, hp)
```

    ## # A tibble: 3 × 4
    ##   week_nr n_responses n_new_cases inc_cases
    ##     <dbl>       <int>       <dbl>     <dbl>
    ## 1       1           2           0         0
    ## 2       2           2           0         0
    ## 3       3           2           2         1

As with prevalence, we can calculate the mean incidence.

``` r
calc_incidence_mean(d_injuries, id_participant, date_sent, injury)
```

    ## # A tibble: 1 × 4
    ##   inc_mean inc_sd inc_ci_lower inc_ci_upper
    ##      <dbl>  <dbl>        <dbl>        <dbl>
    ## 1    0.208 0.0589       0.0620        0.355

And obtain the mean incidence for each health problem type, for a group.

``` r
calc_incidence_all(d_injuries, id_participant, date_sent, c("injury", "injury_substantial"), "gender")
```

    ## # A tibble: 4 × 6
    ##   gender hp_type            inc_mean inc_sd inc_ci_lower inc_ci_upper
    ##   <chr>  <chr>                 <dbl>  <dbl>        <dbl>        <dbl>
    ## 1 Male   injury                0.167  0.236       -0.419        0.752
    ## 2 Female injury                0.25   0.354       -0.628        1.13 
    ## 3 Male   injury_substantial    0.111  0.192       -0.367        0.589
    ## 4 Female injury_substantial    0      0            0            0

#### Find and add event IDs

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
