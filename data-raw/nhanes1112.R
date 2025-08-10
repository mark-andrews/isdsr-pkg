## code to prepare `nhanes_counts` dataset goes here
library(dplyr)

data("NHANES", package = "NHANES")

nhanes1112 <- NHANES %>%
  # book-wide restriction used in the chapter
  filter(Age >= 18, SurveyYr == "2011_12") %>%
  # keep only variables used in analyses
  select(
    Age, # predictor + offset
    DaysPhysHlthBad, # Poisson / quasi-Poisson / negbin outcome
    AlcoholYear, # ZIP outcome
    SexNumPartnLife, # offset-model outcome
    SexAge # offset-model predictor
  ) |>
  # drop rows with all-outcomes-missing to trim size:
  filter(!(is.na(DaysPhysHlthBad) & is.na(AlcoholYear) & is.na(SexNumPartnLife)))

# Store as internal data object in package
usethis::use_data(nhanes1112, overwrite = TRUE)
