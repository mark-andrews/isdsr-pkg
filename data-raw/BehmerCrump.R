# Raw data found at 
# https://raw.githubusercontent.com/CrumpLab/statistics/master/data/exp1_BehmerCrumpAPP.csv
# https://github.com/CrumpLab/statistics.git
# Commit 89415bd108dac9e843a4be60c051ee21a83948b0, 1 Aug 2018 

# This is, as stated at https://github.com/CrumpLab/statistics/blob/cd0e4cc5f62aeb4a0f6e1ad8dfe9c50c4663566a/08-RMANOVA.qmd#L499-L505, from the following paper:
# Behmer, L. P., & Crump, M. J. C. (2017). Spatial Knowledge during Skilled
# Action Sequencing: Hierarchical versus Nonhierarchical Representations.
# Attention, Perception, & Psychophysics, 79(8), 2435–2448.
# doi:10.3758/s13414-017-1389-3
# https://link.springer.com/article/10.3758/s13414-017-1389-3

# @article{behmer2017spatial,
#   title = {Spatial Knowledge during Skilled Action Sequencing: {{Hierarchical}} versus Nonhierarchical Representations},
#   author = {Behmer, Lawrence P and Crump, Matthew JC},
#   year = {2017},
#   journal = {Attention, Perception, \& Psychophysics},
#   volume = {79},
#   number = {8},
#   pages = {2435--2448},
#   publisher = {{Springer}},
#   doi = {10.3758/s13414-017-1389-3}
# }


library(tidyverse)
library(here)

exp1_data <- 
  read_csv(here("data-raw/raw_data_files/exp1_BehmerCrumpAPP.csv")) %>%
  # the following filtering was used by the authors
  # https://github.com/CrumpLab/statistics/blob/cd0e4cc5f62aeb4a0f6e1ad8dfe9c50c4663566a/08-RMANOVA.qmd#L524
  filter(Order == 1, Correct == 1, PureRTs < 5000) %>%
  group_by(Subject, Block, Stimulus) %>%
  summarise(log_rt = log(mean(PureRTs)), .groups = 'drop') %>% 
  mutate(Block = plyr::mapvalues(Block,
                                 from = c("Baseline", "Manipulation"),
                                 to = c("visible", "covered")),
         Stimulus = str_to_lower(Stimulus)
  ) %>%
  rename('keyboard' = 'Block') %>% 
  rename_with(.fn = str_to_lower) %>% 
  mutate(across(.cols = -log_rt, as.factor)) %>% 
  mutate(stimulus = factor(stimulus, levels = c('normal', 'bigrams', 'random')),
         keyboard = factor(keyboard, levels = c('visible', 'covered'))
  )
  
behmercrump_vis <- exp1_data %>%
  filter(keyboard == 'visible') %>%
  select(-keyboard)

behmercrump <- exp1_data

usethis::use_data(behmercrump, behmercrump_vis, overwrite = TRUE)

