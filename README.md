Oslo Sports Trauma Research Centre R Tools (OSTRC package)
================
Lena Kristin Bache-Mathiesen[^1]Benjamin Clarsen[^2]

# Overview

The OSTRC package is a collection of R functions for working with sports
injury data. Among other functions, the package provides tools for
handling data from the Oslo Sports Trauma Research Centre Questionnaire
on health problems.

## OSTRC questionnaire

The OSTRC questionnaire collects self-reported injury data.  
The functions for handling OSTRC-questionnaire data are generalized to
work with any OSTRC-questionnaire version. We denote these OSTRC-O 1.0,
OSTRC-H 1.0, and OSTRC-H 2.0, respectively. Based on the articles they
were introduced, in chronological order from the first to last version:

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
The review panel meeting update: [OSTRC-H
2.0](doi.org/10.1136/bjsports-2019-101337)
</li>
</ol>

# Installation

``` r
# Requires the devtools package
# If devtools is not installed, run code:
# install.packages("devtools") before installing ostrc
devtools::install_github("lenakba/ostrc")
```

# Functions

Below is a brief overview of helpful functions.

## add_event_id

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
