# Artificial data to use as examples of extdata files
library(tidyverse)
library(writexl)

set.seed(42)

## ------------------------------------------------------------------
## 1. A micro-survey on screen use and well-being   →  survey.csv
## ------------------------------------------------------------------
screensurvey <- tibble(
  id = 1:60,
  age = sample(18:65, 60, replace = TRUE),
  gender = sample(c("F", "M"), 60, replace = TRUE, prob = c(.52, .48)),
  scr_hrs = round(rlnorm(60, 1.4, .6), 1), # daily screen-time hours
  happy10 = pmin(10, pmax(
    1, # happiness score, 1–10
    round(7 - .25 * scr_hrs + rnorm(60, 0, 1))
  ))
)

write_csv(screensurvey, "inst/extdata/screensurvey.csv") # saves in your working directory


## ------------------------------------------------------------------
## 2. A toy workplace data set                     →  worklife.xlsx
## ------------------------------------------------------------------
worklife <- tibble(
  emp_id = str_c("E", sprintf("%03d", 1:45)),
  dept = sample(c("HR", "Fin", "Mktg", "Tech"), 45,
    replace = TRUE,
    prob = c(.15, .2, .25, .4)
  ),
  tenure = round(runif(45, .2, 12), 1), # years in post
  remote = sample(c("yes", "no"), 45, replace = TRUE, prob = c(.4, .6)),
  stress5 = sample(1:5, 45, replace = TRUE), # perceived stress, 1–5
)

write_xlsx(worklife, "inst/extdata/worklife.xlsx") # creates a simple one-sheet workbook
