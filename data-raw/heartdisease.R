## code to prepare `heartdisease` dataset
library(tidyverse)

URL <- "https://code.datasciencedojo.com/datasciencedojo/datasets/raw/master/Heart%20Disease/processed.cleveland.data"

heartdisease <- read_csv(
  URL,
  col_names = FALSE,
  na = "?",
  col_select = c(4, 9, 14), # only the columns we need
  col_types = cols(.default = col_double()) # numeric; NA from "?" handled
) |>
  transmute(
    resting_bp = `X4`,
    angina = case_when(
      `X9` == 1 ~ "yes",
      `X9` == 0 ~ "no",
      TRUE ~ NA_character_
    ) |> factor(levels = c("no", "yes")),
    heart_disease = case_when(
      `X14` > 0 ~ "yes", # 1–4 = presence
      `X14` == 0 ~ "no",
      TRUE ~ NA_character_
    ) |> factor(levels = c("no", "yes"))
  )

usethis::use_data(heartdisease, overwrite = TRUE)
