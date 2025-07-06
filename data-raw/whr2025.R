## code to prepare `owidwhr` and `whr2025` datasets
library(tidyverse)

# raw data from https://ourworldindata.org/grapher/gdp-vs-happiness
owidwhr <- read_csv("data-raw/raw_data_files/gdp-vs-happiness.csv") |>
  select(
    country = Entity,
    iso3c = Code,
    year = Year,
    happiness = `Cantril ladder score`,
    gdp = `GDP per capita, PPP (constant 2021 international $)`
  ) |>
  mutate(lgdp = log(gdp)) |>
  drop_na() |>
  filter(country != "World")

# World happiness report 2025; data year = 2023
whr2025 <- filter(owidwhr, year == 2023) |>
  select(-year) |>
  mutate(
    income = ntile(gdp, 3),
    income = plyr::mapvalues(income, from = 1:3, to = c("low", "medium", "high")) |> factor(ordered = TRUE, levels = c("low", "medium", "high"))
  )

usethis::use_data(owidwhr, overwrite = TRUE)
usethis::use_data(whr2025, overwrite = TRUE)
