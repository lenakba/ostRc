README
================
Lena Kristin Bache-Mathiesen

# Overview

The OSTRC package is a collection of tools for working with sports
injury data.

# Installation

``` r
# Requires the devtools package
# If devtools is not installed, run code:
# install.packages("devtools") before installing ostrc
devtools::install_github("lenakba/ostrc")
```

# Functions

Below is a brief overview of helpful functions.

## add_micro_cycle

Micro-cycles are the time from a previous competition or match to the
next competition or match. Coaches often plan training according to such
micro-cycles. For longitudinal data, `add_micro_cycle` finds the
micro-cycles given the date, and the event (match or competition). This
is handy for counting the number of micro-cycles or calculating the
average of a variable per micro-cycle, among other iterations.  
This can also be used to find each injury interval in the data.

``` r
d_matches_sim = tribble(~date, ~match,
                          "2017-08-22", 0,
                          "2017-08-23", 1,
                          "2017-08-24", 0,
                          "2017-08-25", 0,
                          "2017-08-26", 1)
  d_matches_sim = d_matches_sim %>% mutate(date = lubridate::as_date(date))
  add_micro_cycle(d_matches_sim, date, match)
```

    ## # A tibble: 5 × 3
    ##   date       match micro_cycle
    ##   <date>     <dbl> <chr>      
    ## 1 2017-08-22     0 1          
    ## 2 2017-08-23     1 1          
    ## 3 2017-08-24     0 2          
    ## 4 2017-08-25     0 2          
    ## 5 2017-08-26     1 2
