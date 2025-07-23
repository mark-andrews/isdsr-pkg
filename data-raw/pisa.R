## code to prepare `pisa` dataset goes here
# learn more about Programme for International Student Assessment (PISA)
# https://www.oecd.org/pisa/aboutpisa/

# install devtools with install.packages("devtools") if necessary
devtools::install_github("kevinwang09/learningtower")


student_all <- learningtower::load_student("all")

pisa <- student_all |>
  drop_na(year, country, gender) |>
  summarise(across(math:science, ~ mean(., na.rm = TRUE)),
    .by = c(year, country, gender)
  ) |>
  left_join(learningtower::countrycode) |>
  relocate(country_name, .after = country) |>
  # because some NaNs creep in
  drop_na()

# data from UK in 2022
pisa2022uk <- student_all |>
  filter(year == 2022, country == "GBR") |>
  select(math, read, science, gender) |>
  drop_na()

usethis::use_data(pisa, overwrite = TRUE)
usethis::use_data(pisa2022uk, overwrite = TRUE)
