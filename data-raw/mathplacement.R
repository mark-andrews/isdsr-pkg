## code to prepare `mathplacement` dataset goes here

library(tidyverse)

data(MathPlacement, package = 'Stat2Data')

mathplacement <- select(MathPlacement, score = PlcmtScore) %>% 
  mutate(score = as.numeric(score)) %>% 
  as_tibble()

usethis::use_data(mathplacement, overwrite = TRUE)
