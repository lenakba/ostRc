# Overview
The OSTRC package is a collection of tools for working with sports injury data.

# Installation

```
# Requires the devtools package
# If devtools is not installed, run code below:
# install.packages("devtools")
devtools::install_github("lenakba/ostrc")
```

# Functions

## add_micro_cycle
Micro-cycles are the time from a previous competition or match
to the next competition or match. Coaches often plan training according to such 
micro-cycles.
`add_micro_cycle` adds an index for the micro-cycle number 
for longitudinal data with 
matches or competition events. 
This is handy for counting the number of micro-cycles
or calculating the average of a variable per micro-cycle, 
among other iterations.   

```
d_matches_sim = tribble(~date, ~match,
                          "2017-08-22", 0,
                          "2017-08-23", 1,
                          "2017-08-24", 0,
                          "2017-08-25", 0,
                          "2017-08-26", 1)
  d_matches_sim = d_matches_sim %>% mutate(date = lubridate::as_date(date))
  add_micro_cycle(d_matches_sim, date, match)
```