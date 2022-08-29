# Pomiechowska B. & Gliga T. (2021) Nonverbal category knowledge limits
# the amount of information encoded in object representations: EEG evidence from
# 12-month-old infants. Royal Society Open Science. 8: 200782. 
# https://doi.org/10.1098/rsos.200782
# Accompanying materials on GitHub: https://github.com/bpomie/remember

# @article{pomiechowska2021nonverbal,
#   title={Nonverbal category knowledge limits the amount of information encoded in object representations: EEG evidence from 12-month-old infants},
#   author={Pomiechowska, Barbara and Gliga, Teodora},
#   journal={Royal Society open science},
#   volume={8},
#   number={3},
#   pages={200782},
#   year={2021},
#   publisher={The Royal Society}
# }

# Raw data file `data_erp.xlsx` taken from master/data/data_erp.xlsx
# Repository: https://github.com/bpomie/remember.git
# Commit: 6eba1c3644e94b6bca6f5b84d7b595f852d3355c
# Renamed here as `pomiechowska2021nonverbal_data_erp.xlsx`

# Data is also available at OSF: https://osf.io/652cp/
# https://osf.io/652cp/files/github/data%2Fdata_erp.xlsx

library(tidyverse)
library(here)
library(stringr)

catknowledge <- 
  readxl::read_excel(
    here('data-raw/raw_data_files/pomiechowska2021nonverbal_data_erp.xlsx')
  ) %>% 
  filter(Experiment == 'Experiment 1') %>%
  select(-Experiment) %>% 
  pivot_longer(cols = starts_with('NC_change'),
               names_to = 'change',
               names_prefix = 'NC_change',
               values_to = 'nc_erp') %>% 
  # lowercase the values in Category and change
  mutate(across(Category:change, str_to_lower)) %>% 
  # lowercase all variable names
  rename_with(str_to_lower) %>% 
  # all but nc_erp are factors
  mutate(across(-nc_erp, as.factor)) %>% 
  # set the levels of the change variable to `no`, `across`, `within`
  mutate(change = factor(change, levels = c("no", "across", "within")))
         


usethis::use_data(catknowledge, overwrite = TRUE)
