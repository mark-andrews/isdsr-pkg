library(dplyr)
library(tidyr)
library(readr)

# YouGov YouGov / VICE Survey
# Sample Size: 1300 British 16-24 year olds
# Fieldwork: 27th June - 5th July 2017

# Original report: 
# https://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/h5xr6v0nr4/VICEResults_170706_Drugs_16-24_W.pdf

# pdftotext output of pdf report available in raw_data_files/VICEResults_170706_Drugs_16-24_W.txt

# The following is taken from second percentages table on page 2, plus the sample size break down on the top of page.
# Percentages of responses to question: Have you ever taken an illegal drug?

response_levels <- c("I have taken illegal drugs frequently and still do",
                     "I have taken illegal drugs frequently but have now stopped completely",
                     "I have taken illegal drugs occasionally and still do",
                     "I have taken illegal drugs occasionally but have now stopped completely",
                     "I have only taken illegal drugs once or twice",
                     "I’ve never taken any illegal drugs",
                     "Not sure",
                     "Prefer not to say")

age_levels <- c("16-18", "19-21", "22-24")

yougov17drugs <- read_delim(I("response;total;male;female;16-18;19-21;22-24;m_16-18;m_19-21;m_22-24;f_16-18;f_19-21;f_22-24;abc1;c2de;london;south;midlands/wales;north;scotland
I have taken illegal drugs frequently and still do;4;5;2;2;3;6;1;5;8;2;2;3;4;3;2;3;2;3;14
I have taken illegal drugs frequently but have now stopped completely;3;2;3;1;2;5;2;1;4;1;4;5;3;2;3;2;4;4;0
I have taken illegal drugs occasionally and still do;11;13;9;9;11;13;12;12;16;6;10;10;12;10;13;10;10;13;9
I have taken illegal drugs occasionally but have now stopped completely;6;6;7;3;5;10;4;4;10;3;7;10;5;8;9;6;5;8;3
I have only taken illegal drugs once or twice;14;15;14;12;12;19;12;10;20;12;14;17;17;12;12;15;17;13;13
TOTAL TAKEN ILLEGAL DRUGS;20;21;21;15;17;29;16;14;30;15;21;27;22;20;21;21;22;21;16
I’ve never taken any illegal drugs;57;55;59;68;60;44;67;62;36;69;58;51;55;59;49;61;56;56;58
Not sure;2;2;2;1;2;2;0;1;3;3;3;1;1;3;3;1;3;1;1
Prefer not to say;3;3;3;3;4;2;1;6;2;5;3;2;3;3;9;2;3;2;1"), 
           delim=';') %>% 
  select(response, matches('^[mf]_')) %>% 
  pivot_longer(cols = -response, 
               names_to = c('gender', 'age'), 
               names_sep = '_',
               values_to = 'percentage') %>% 
  filter(response != 'TOTAL TAKEN ILLEGAL DRUGS') %>% 
  left_join(
    read_delim(I("
sample;total;male;female;16-18;19-21;22-24;m_16-18;m_19-21;m_22-24;f_16-18;f_19-21;f_22-24;abc1;c2de;london;south;midlands/wales;north;scotland
Weighted;1300;663;637;404;436;460;208;221;234;196;214;226;653;647;173;398;289;329;112
Unweighted;1300;431;869;450;416;434;162;136;133;288;280;301;850;450;171;424;304;314;87
             "), delim=';') %>%
      select(sample, matches('^[mf]_')) %>%
      pivot_longer(cols = -sample,
                   names_to = c('gender', 'age'),
                   names_sep = '_',
                   values_to = 'count') %>%
      pivot_wider(names_from = sample, values_from = count)
  ) %>% mutate(response = factor(response, levels = response_levels),
               age = factor(age, levels = age_levels, ordered = T),
               gender = case_when(
                 gender == 'm' ~ 'male',
                 gender == 'f' ~ 'female')
  )

# Convert from percentages to counts --------------------------------------

yougov17drugs <- yougov17drugs %>%
  mutate(count = round(Unweighted * percentage/100)) %>% 
  select(gender, age, response, count)

# Add to package data -----------------------------------------------------

usethis::use_data(yougov17drugs, overwrite = TRUE)
