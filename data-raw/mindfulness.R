library(tidyverse)

# The xlsx file below was obtained at
# https://datadryad.org/dataset/doi:10.5061/dryad.f4688
# Data from: Mindfulness online: a preliminary evaluation of the
# feasibility of a web-based mindfulness
# course and the impact on stress on 29 June 2025

mindfulness <- readxl::read_excel("data-raw/raw_data_files/BMO Dryad Data.xlsx", skip = 1) |>
  select(gender = Gender, contains("pss")) |>
  rename(pss_before = `PSS - Before Course`, pss_after = `PSS - Course Completed`, pss_followup = `PSS - One Month On`) |>
  rowid_to_column(var = "id") |>
  mutate(across(id:gender, as.factor))


usethis::use_data(mindfulness, overwrite = TRUE, compress = "xz")
