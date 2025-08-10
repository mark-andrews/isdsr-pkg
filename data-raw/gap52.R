## code to prepare `gap52` dataset goes here

data(package = "gapminder")
gap52 <- gapminder |>
  dplyr::filter(year == 1952, continent != "Oceania") |>
  droplevels()

usethis::use_data(gap52, overwrite = TRUE)
