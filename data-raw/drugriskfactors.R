## Code to prepare `drug_personality` dataset

library(readr)
library(dplyr)

# Get and check raw data file ---------------------------------------------

# The raw data is available at the UCI Machine Learning Repository:
# https://archive.ics.uci.edu/ml/machine-learning-databases/00373/drug_consumption.data
# Based on the paper "The Five Factor Model of personality and evaluation of drug consumption risk"
# https://arxiv.org/pdf/1506.06297.pdf
# The `drug_consumption.data` file is a csv file.
# Downloading this file on Dec 28, 2021, at approximately 13:20 GMT, the md5sum is as follows:
# > md5sum drug_consumption.data 
# 257152c2921546d38f2331ab6f32e33a  drug_consumption.data

raw_data_file <- here::here("data-raw/raw_data_files/drug_consumption.data")
raw_data_file_checksum <- '257152c2921546d38f2331ab6f32e33a'

assertthat::assert_that(
  assertthat::are_equal(
    unname(tools::md5sum(raw_data_file)), 
    raw_data_file_checksum)
)

# The raw data csv file has no column labels
raw_data_df <- readr::read_csv(raw_data_file, show_col_types = F,
                               col_names = NULL)


# Rename and convert all columns ------------------------------------------

# All the columns in the raw data are unlabelled. 

# Also, most of the columns have been converted into standardized real numbers,
# even when they are necessarily categorical variables like Country (e.g.UK, US,
# etc). The mapping between the real numbers and the original values of the
# original variables is described in
# https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29

# In what follows, in the comments, we have the original text from that UCI
# webpage. We  read in strings from that page using read_csv and then join with
# the raw data dataframe to obtain the original values.

# Column 1: ID ------------------------------------------------------------

# 1. ID is number of record in original database. Cannot be related to
# participant. It can be used for reference only.

raw_data_df <- rename(raw_data_df, id = X1)


# Column 2: Age -----------------------------------------------------------

# 2. Age (Real) is age of participant and has one of the values:
#   Value Meaning Cases Fraction
# -0.95197 18-24 643 34.11%
# -0.07854 25-34 481 25.52%
# 0.49788 35-44 356 18.89%
# 1.09449 45-54 294 15.60%
# 1.82213 55-64 93 4.93%
# 2.59171 65+ 18 0.95%
# Descriptive statistics
# Min Max Mean Std.dev.
# -0.95197 2.59171 0.03461 0.87813

age_info_df <- read_delim(I("
# Value Meaning Cases Fraction
# -0.95197 18-24 643 34.11%
# -0.07854 25-34 481 25.52%
# 0.49788 35-44 356 18.89%
# 1.09449 45-54 294 15.60%
# 1.82213 55-64 93 4.93%
# 2.59171 65+ 18 0.95%
"), delim = ' ', show_col_types = FALSE) %>% 
  select(Value, age = Meaning)


raw_data_df <- left_join(
  raw_data_df,
  age_info_df,
  by = c("X2"="Value")
) %>% select(-X2) %>% relocate(age, .after = id)

# Column 3: Gender --------------------------------------------------------

# 3. Gender (Real) is gender of participant:
#   Value Meaning Cases Fraction
# 0.48246 Female 942 49.97%
# -0.48246 Male 943 50.03%
# Descriptive statistics
# Min Max Mean Std.dev.
# -0.48246 0.48246 -0.00026 0.48246

gender_info_df <- read_delim(I("
# Value Meaning Cases Fraction
# 0.48246 Female 942 49.97%
# -0.48246 Male 943 50.03%
"), delim = ' ', show_col_types = FALSE) %>% 
  select(Value, gender = Meaning)

raw_data_df <- left_join(
  raw_data_df,
  gender_info_df,
  by = c("X3"="Value")
) %>% select(-X3) %>% relocate(gender, .after = age)


# Column 4: Education -----------------------------------------------------

# 4. Education (Real) is level of education of participant and has one of the values:
#   Value Meaning Cases Fraction
# -2.43591 Left school before 16 years 28 1.49%
# -1.73790 Left school at 16 years 99 5.25%
# -1.43719 Left school at 17 years 30 1.59%
# -1.22751 Left school at 18 years 100 5.31%
# -0.61113 Some college or university, no certificate or degree 506 26.84%
# -0.05921 Professional certificate/ diploma 270 14.32%
# 0.45468 University degree 480 25.46%
# 1.16365 Masters degree 283 15.01%
# 1.98437 Doctorate degree 89 4.72%
# Descriptive statistics
# Min Max Mean Std.dev.
# -2.43591 1.98437 -0.00379 0.95004


education_info_df <- read_csv(I('
#, Value, Meaning, Cases, Fraction
#, -2.43591, "Left school before 16 years", 28, 1.49%
#, -1.73790, "Left school at 16 years", 99, 5.25%
#, -1.43719, "Left school at 17 years", 30, 1.59%
#, -1.22751, "Left school at 18 years", 100, 5.31%
#, -0.61113, "Some college or university; no certificate or degree", 506, 26.84%
#, -0.05921, "Professional certificate/ diploma", 270, 14.32%
#, 0.45468, "University degree", 480, 25.46%
#, 1.16365, "Masters degree", 283, 15.01%
#, 1.98437, "Doctorate degree", 89, 4.72%
')) %>% select(Value, education = Meaning)

raw_data_df <- left_join(
  raw_data_df,
  education_info_df,
  by = c("X4"="Value")
) %>% select(-X4) %>% relocate(education, .after = gender)



# Column 5: Country -------------------------------------------------------


# 5. Country (Real) is country of current residence of participant and has one of the values:
#   Value Meaning Cases Fraction
# -0.09765 Australia 54 2.86%
# 0.24923 Canada 87 4.62%
# -0.46841 New Zealand 5 0.27%
# -0.28519 Other 118 6.26%
# 0.21128 Republic of Ireland 20 1.06%
# 0.96082 UK 1044 55.38%
# -0.57009 USA 557 29.55%
# Descriptive statistics
# Min Max Mean Std.dev.
# -0.57009 0.96082 0.35554 0.70015


country_info_df <- read_delim(I("
#, Value, Meaning, Cases, Fraction
#, -0.09765, Australia, 54, 2.86%
#, 0.24923, Canada, 87, 4.62%
#, -0.46841, 'New Zealand', 5, 0.27%
#, -0.28519, Other, 118, 6.26%
#, 0.21128, 'Republic of Ireland', 20, 1.06%
#, 0.96082, UK, 1044, 55.38%
#, -0.57009, USA, 557, 29.55%
"), delim = ', ', show_col_types = FALSE) %>% 
  select(Value, country = Meaning)

raw_data_df <- left_join(
  raw_data_df,
  country_info_df,
  by = c("X5"="Value")
) %>% select(-X5) %>% relocate(country, .after = education)


# Column 6: Ethnicity -----------------------------------------------------

# 6. Ethnicity (Real) is ethnicity of participant and has one of the values:
#   Value Meaning Cases Fraction
# -0.50212 Asian 26 1.38%
# -1.10702 Black 33 1.75%
# 1.90725 Mixed-Black/Asian 3 0.16%
# 0.12600 Mixed-White/Asian 20 1.06%
# -0.22166 Mixed-White/Black 20 1.06%
# 0.11440 Other 63 3.34%
# -0.31685 White 1720 91.25%
# Descriptive statistics
# Min Max Mean Std.dev.
# -1.10702 1.90725 -0.30958 0.16618


ethnicity_info_df <- read_delim(I("
# Value Meaning Cases Fraction
# -0.50212 Asian 26 1.38%
# -1.10702 Black 33 1.75%
# 1.90725 Mixed-Black/Asian 3 0.16%
# 0.12600 Mixed-White/Asian 20 1.06%
# -0.22166 Mixed-White/Black 20 1.06%
# 0.11440 Other 63 3.34%
# -0.31685 White 1720 91.25%
"), delim = ' ', show_col_types = FALSE) %>% 
  select(Value, ethnicity = Meaning)

raw_data_df <- left_join(
  raw_data_df,
  ethnicity_info_df,
  by = c("X6"="Value")
) %>% select(-X6) %>% relocate(ethnicity, .after = country)

# Column 7: Neuroticism ---------------------------------------------------

# 7. Nscore (Real) is NEO-FFI-R Neuroticism. Possible values are presented in table below:
#   Nscore Cases Value Nscore Cases Value Nscore Cases Value
# 12 1 -3.46436 29 60 -0.67825 46 67 1.02119
# 13 1 -3.15735 30 61 -0.58016 47 27 1.13281
# 14 7 -2.75696 31 87 -0.46725 48 49 1.23461
# 15 4 -2.52197 32 78 -0.34799 49 40 1.37297
# 16 3 -2.42317 33 68 -0.24649 50 24 1.49158
# 17 4 -2.34360 34 76 -0.14882 51 27 1.60383
# 18 10 -2.21844 35 69 -0.05188 52 17 1.72012
# 19 16 -2.05048 36 73 0.04257 53 20 1.83990
# 20 24 -1.86962 37 67 0.13606 54 15 1.98437
# 21 31 -1.69163 38 63 0.22393 55 11 2.12700
# 22 26 -1.55078 39 66 0.31287 56 10 2.28554
# 23 29 -1.43907 40 80 0.41667 57 6 2.46262
# 24 35 -1.32828 41 61 0.52135 58 3 2.61139
# 25 56 -1.19430 42 77 0.62967 59 5 2.82196
# 26 57 -1.05308 43 49 0.73545 60 2 3.27393
# 27 65 -0.92104 44 51 0.82562
# 28 70 -0.79151 45 37 0.91093
# Descriptive statistics
# Min Max Mean Std.dev.
# -3.46436 3.27393 0.00004 0.99808

neuroticism_info_df <- read_table(I("
Nscore Cases Value
12 1 -3.46436
13 1 -3.15735
14 7 -2.75696
15 4 -2.52197
16 3 -2.42317
17 4 -2.34360
18 10 -2.21844
19 16 -2.05048
20 24 -1.86962
21 31 -1.69163
22 26 -1.55078
23 29 -1.43907
24 35 -1.32828
25 56 -1.19430
26 57 -1.05308
27 65 -0.92104
28 70 -0.79151
29 60 -0.67825
30 61 -0.58016
31 87 -0.46725
32 78 -0.34799
33 68 -0.24649
34 76 -0.14882
35 69 -0.05188
36 73 0.04257
37 67 0.13606
38 63 0.22393
39 66 0.31287
40 80 0.41667
41 61 0.52135
42 77 0.62967
43 49 0.73545
44 51 0.82562
45 37 0.91093
46 67 1.02119
47 27 1.13281
48 49 1.23461
49 40 1.37297
50 24 1.49158
51 27 1.60383
52 17 1.72012
53 20 1.83990
54 15 1.98437
55 11 2.12700
56 10 2.28554
57 6 2.46262
58 3 2.61139
59 5 2.82196
60 2 3.27393")) %>% 
  select(Value, neuroticism=Nscore)

raw_data_df <- left_join(
  raw_data_df,
  neuroticism_info_df,
  by = c("X7"="Value")
) %>% select(-X7) %>% relocate(neuroticism, .after = ethnicity)


# Column 8: Extraversion ----------------------------------------------------

# 8. Escore (Real) is NEO-FFI-R Extraversion. Possible values are presented in table below:
#   Escore Cases Value Escore Cases Value Escore Cases Value
# 16 2 -3.27393 31 55 -1.23177 45 91 0.80523
# 18 1 -3.00537 32 52 -1.09207 46 69 0.96248
# 19 6 -2.72827 33 77 -0.94779 47 64 1.11406
# 20 3 -2.53830 34 68 -0.80615 48 62 1.28610
# 21 3 -2.44904 35 58 -0.69509 49 37 1.45421
# 22 8 -2.32338 36 89 -0.57545 50 25 1.58487
# 23 5 -2.21069 37 90 -0.43999 51 34 1.74091
# 24 9 -2.11437 38 106 -0.30033 52 21 1.93886
# 25 4 -2.03972 39 107 -0.15487 53 15 2.12700
# 26 21 -1.92173 40 130 0.00332 54 10 2.32338
# 27 23 -1.76250 41 116 0.16767 55 9 2.57309
# 28 23 -1.63340 42 109 0.32197 56 2 2.85950
# 29 32 -1.50796 43 105 0.47617 58 1 3.00537
# 30 38 -1.37639 44 103 0.63779 59 2 3.27393
# Descriptive statistics
# Min Max Mean Std.dev.
# -3.27393 3.27393 -0.00016 0.99745


extraversion_info_df <- read_table(I("
Escore Cases Value
16 2 -3.27393
18 1 -3.00537
19 6 -2.72827
20 3 -2.53830
21 3 -2.44904
22 8 -2.32338
23 5 -2.21069
24 9 -2.11437
25 4 -2.03972
26 21 -1.92173
27 23 -1.76250
28 23 -1.63340
29 32 -1.50796
30 38 -1.37639
31 55 -1.23177
32 52 -1.09207
33 77 -0.94779
34 68 -0.80615
35 58 -0.69509
36 89 -0.57545
37 90 -0.43999
38 106 -0.30033
39 107 -0.15487
40 130 0.00332
41 116 0.16767
42 109 0.32197
43 105 0.47617
44 103 0.63779
45 91 0.80523
46 69 0.96248
47 64 1.11406
48 62 1.28610
49 37 1.45421
50 25 1.58487
51 34 1.74091
52 21 1.93886
53 15 2.12700
54 10 2.32338
55 9 2.57309
56 2 2.85950
58 1 3.00537
59 2 3.27393")) %>% 
  select(Value, extraversion=Escore)


raw_data_df <- left_join(
  raw_data_df,
  extraversion_info_df,
  by = c("X8"="Value")
) %>% select(-X8) %>% relocate(extraversion, .after = neuroticism)


# Column 9: Openness ------------------------------------------------------

# 9. Oscore (Real) is NEO-FFI-R Openness to experience. Possible values are presented in table below:
#   Oscore Cases Value Oscore Cases Value Oscore Cases Value
# 24 2 -3.27393 38 64 -1.11902 50 83 0.58331
# 26 4 -2.85950 39 60 -0.97631 51 87 0.72330
# 28 4 -2.63199 40 68 -0.84732 52 87 0.88309
# 29 11 -2.39883 41 76 -0.71727 53 81 1.06238
# 30 9 -2.21069 42 87 -0.58331 54 57 1.24033
# 31 9 -2.09015 43 86 -0.45174 55 63 1.43533
# 32 13 -1.97495 44 101 -0.31776 56 38 1.65653
# 33 23 -1.82919 45 103 -0.17779 57 34 1.88511
# 34 25 -1.68062 46 134 -0.01928 58 19 2.15324
# 35 26 -1.55521 47 107 0.14143 59 13 2.44904
# 36 39 -1.42424 48 116 0.29338 60 7 2.90161
# 37 51 -1.27553 49 98 0.44585
# Descriptive statistics
# Min Max Mean Std.dev.
# -3.27393 2.90161 -0.00053 0.99623


openness_info_df <- read_table(I("
Oscore Cases Value
24 2 -3.27393
26 4 -2.85950
28 4 -2.63199
29 11 -2.39883
30 9 -2.21069
31 9 -2.09015
32 13 -1.97495
33 23 -1.82919
34 25 -1.68062
35 26 -1.55521
36 39 -1.42424
37 51 -1.27553
38 64 -1.11902
39 60 -0.97631
40 68 -0.84732
41 76 -0.71727
42 87 -0.58331
43 86 -0.45174
44 101 -0.31776
45 103 -0.17779
46 134 -0.01928
47 107 0.14143
48 116 0.29338
49 98 0.44585
50 83 0.58331
51 87 0.72330
52 87 0.88309
53 81 1.06238
54 57 1.24033
55 63 1.43533
56 38 1.65653
57 34 1.88511
58 19 2.15324
59 13 2.44904
60 7 2.90161")) %>% 
  select(Value, openness=Oscore)

raw_data_df <- left_join(
  raw_data_df,
  openness_info_df,
  by = c("X9"="Value")
) %>% select(-X9) %>% relocate(openness, .after = extraversion)


# Column 10: Agreeableness ------------------------------------------------

# 10. Ascore (Real) is NEO-FFI-R Agreeableness. Possible values are presented in table below:
#   Ascore Cases Value Ascore Cases Value Ascore Cases Value
# 12 1 -3.46436 34 42 -1.34289 48 104 0.76096
# 16 1 -3.15735 35 45 -1.21213 49 85 0.94156
# 18 1 -3.00537 36 62 -1.07533 50 68 1.11406
# 23 1 -2.90161 37 83 -0.91699 51 58 1.2861
# 24 2 -2.78793 38 82 -0.76096 52 39 1.45039
# 25 1 -2.70172 39 102 -0.60633 53 36 1.61108
# 26 7 -2.53830 40 98 -0.45321 54 36 1.81866
# 27 7 -2.35413 41 114 -0.30172 55 16 2.03972
# 28 8 -2.21844 42 101 -0.15487 56 14 2.23427
# 29 13 -2.07848 43 105 -0.01729 57 8 2.46262
# 30 18 -1.92595 44 118 0.13136 58 7 2.75696
# 31 24 -1.77200 45 112 0.28783 59 1 3.15735
# 32 30 -1.62090 46 100 0.43852 60 1 3.46436
# 33 34 -1.47955 47 100 0.59042
# Descriptive statistics
# Min Max Mean Std.dev.
# -3.46436 3.46436 -0.00024 0.99744


agreeableness_info_df <- read_table(I("
Ascore Cases Value
12 1 -3.46436
16 1 -3.15735
18 1 -3.00537
23 1 -2.90161
24 2 -2.78793
25 1 -2.70172
26 7 -2.53830
27 7 -2.35413
28 8 -2.21844
29 13 -2.07848
30 18 -1.92595
31 24 -1.77200
32 30 -1.62090
33 34 -1.47955
34 42 -1.34289
35 45 -1.21213
36 62 -1.07533
37 83 -0.91699
38 82 -0.76096
39 102 -0.60633
40 98 -0.45321
41 114 -0.30172
42 101 -0.15487
43 105 -0.01729
44 118 0.13136
45 112 0.28783
46 100 0.43852
47 100 0.59042
48 104 0.76096
49 85 0.94156
50 68 1.11406
51 58 1.2861
52 39 1.45039
53 36 1.61108
54 36 1.81866
55 16 2.03972
56 14 2.23427
57 8 2.46262
58 7 2.75696
59 1 3.15735
60 1 3.46436")) %>% 
  select(Value, agreeableness=Ascore)

raw_data_df <- left_join(
  raw_data_df,
  agreeableness_info_df,
  by = c("X10"="Value")
) %>% select(-X10) %>% relocate(agreeableness, .after = openness)

# Column 11: Conscientiousness --------------------------------------------

# 11. Cscore (Real) is NEO-FFI-R Conscientiousness. Possible values are presented in table below:
#   Cscore Cases Value Cscore Cases Value Cscore Cases Value
# 17 1 -3.46436 32 39 -1.25773 46 113 0.58489
# 19 1 -3.15735 33 49 -1.13788 47 95 0.7583
# 20 3 -2.90161 34 55 -1.01450 48 95 0.93949
# 21 2 -2.72827 35 55 -0.89891 49 76 1.13407
# 22 5 -2.57309 36 69 -0.78155 50 47 1.30612
# 23 5 -2.42317 37 81 -0.65253 51 43 1.46191
# 24 6 -2.30408 38 77 -0.52745 52 34 1.63088
# 25 9 -2.18109 39 87 -0.40581 53 28 1.81175
# 26 13 -2.04506 40 97 -0.27607 54 27 2.04506
# 27 13 -1.92173 41 99 -0.14277 55 13 2.33337
# 28 25 -1.78169 42 105 -0.00665 56 8 2.63199
# 29 24 -1.64101 43 90 0.12331 57 3 3.00537
# 30 29 -1.51840 44 111 0.25953 59 1 3.46436
# 31 41 -1.38502 45 111 0.41594
# Descriptive statistics
# Min Max Mean Std.dev.
# -3.46436 3.46436 -0.00039 0.99752


conscientiousness_info_df <- read_table(I("
Cscore Cases Value
17 1 -3.46436
19 1 -3.15735
20 3 -2.90161
21 2 -2.72827
22 5 -2.57309
23 5 -2.42317
24 6 -2.30408
25 9 -2.18109
26 13 -2.04506
27 13 -1.92173
28 25 -1.78169
29 24 -1.64101
30 29 -1.51840
31 41 -1.38502
32 39 -1.25773
33 49 -1.13788
34 55 -1.01450
35 55 -0.89891
36 69 -0.78155
37 81 -0.65253
38 77 -0.52745
39 87 -0.40581
40 97 -0.27607
41 99 -0.14277
42 105 -0.00665
43 90 0.12331
44 111 0.25953
45 111 0.41594
46 113 0.58489
47 95 0.7583
48 95 0.93949
49 76 1.13407
50 47 1.30612
51 43 1.46191
52 34 1.63088
53 28 1.81175
54 27 2.04506
55 13 2.33337
56 8 2.63199
57 3 3.00537
59 1 3.46436")) %>% 
  select(Value, conscientiousness=Cscore)

raw_data_df <- left_join(
  raw_data_df,
  conscientiousness_info_df,
  by = c("X11"="Value")
) %>% select(-X11) %>% relocate(conscientiousness, .after = agreeableness)



# Column 12: Impulsiveness ------------------------------------------------




# 12. Impulsive (Real) is impulsiveness measured by BIS-11. Possible values are presented in table below:
#   Impulsiveness Cases Fraction
# -2.55524 20 1.06%
# -1.37983 276 14.64%
# -0.71126 307 16.29%
# -0.21712 355 18.83%
# 0.19268 257 13.63%
# 0.52975 216 11.46%
# 0.88113 195 10.34%
# 1.29221 148 7.85%
# 1.86203 104 5.52%
# 2.90161 7 0.37%
# Descriptive statistics
# Min Max Mean Std.dev.
# -2.55524 2.90161 0.00721 0.95446
 
# The original values of impulsiveness are not reported on the UCI webpage
# However, if we go the 36536-001-Codebook.pdf page 12, we see that impulsivity 
# original values go from 0 to 9


impulsiveness_info_df <- read_table(I("
# impuls Impulsiveness Cases Fraction
# 0 -2.55524 20 1.06%
# 1 -1.37983 276 14.64%
# 2 -0.71126 307 16.29%
# 3 -0.21712 355 18.83%
# 4 0.19268 257 13.63%
# 5 0.52975 216 11.46%
# 6 0.88113 195 10.34%
# 7 1.29221 148 7.85%
# 8 1.86203 104 5.52%
# 9 2.90161 7 0.37%
")) %>% 
  select(Value=Impulsiveness, impuls)

raw_data_df <- left_join(
  raw_data_df,
  impulsiveness_info_df,
  by = c("X12"="Value")
) %>% select(-X12) %>% relocate(impuls, .after = conscientiousness)

# Column 13: Sensation seeking --------------------------------------------


# 13. SS (Real) is sensation seeing measured by ImpSS. Possible values are presented in table below:
#   SS Cases Fraction
# -2.07848 71 3.77%
# -1.54858 87 4.62%
# -1.18084 132 7.00%
# -0.84637 169 8.97%
# -0.52593 211 11.19%
# -0.21575 223 11.83%
# 0.07987 219 11.62%
# 0.40148 249 13.21%
# 0.76540 211 11.19%
# 1.22470 210 11.14%
# 1.92173 103 5.46%
# Descriptive statistics
# Min Max Mean Std.dev.
# -2.07848 1.92173 -0.00329 0.96370

# 13. SS (Real) is sensation seeing measured by ImpSS. Possible values are presented in table below:

# Descriptive statistics
# Min Max Mean Std.dev.
# -2.07848 1.92173 -0.00329 0.96370



# The original values of impulsiveness are not reported on the UCI webpage
# However, if we go the 36536-001-Codebook.pdf, page12-13, we see that sensation-seeking
# original values go from 0 to 10
senseek_info_df <- read_table(I("
# senseek SS Cases Fraction
# 0 -2.07848 71 3.77%
# 1 -1.54858 87 4.62%
# 2 -1.18084 132 7.00%
# 3 -0.84637 169 8.97%
# 4 -0.52593 211 11.19%
# 5 -0.21575 223 11.83%
# 6 0.07987 219 11.62%
# 7 0.40148 249 13.21%
# 8 0.76540 211 11.19%
# 9 1.22470 210 11.14%
# 10 1.92173 103 5.46%
")) %>% 
  select(Value=SS, senseek)

raw_data_df <- left_join(
  raw_data_df,
  senseek_info_df,
  by = c("X13"="Value")
) %>% select(-X13) %>% relocate(senseek, .after = impuls)




# 14. Alcohol is class of alcohol consumption.
# 15. Amphet is class of amphetamines consumption.
# 16. Amyl is class of amyl nitrite consumption.
# 17. Benzos is class of benzodiazepine consumption.
# 18. Caff is class of caffeine consumption.
# 19. Cannabis is class of cannabis consumption.
# 20. Choc is class of chocolate consumption. 
# 21. Coke is class of cocaine consumption.
# 22. Crack is class of crack consumption.
# 23. Ecstasy is class of ecstasy consumption.
# 24. Heroin is class of heroin consumption. 
# 25. Ketamine is class of ketamine consumption.
# 26. Legalh is class of legal highs consumption. 
# 27. LSD is class of alcohol consumption.
# 28. Meth is class of methadone consumption.
# 29. Mushrooms is class of magic mushrooms consumption.
# 30. Nicotine is class of nicotine consumption.
# 31. Semer is class of fictitious drug Semeron consumption.
# 32. VSA is class of volatile substance abuse consumption.

# Columns 14 to 32 have the values CL0 ...  CL6 with the meanings 

# CL0 Never Used
# CL1 Used over a Decade Ago
# CL2 Used in Last Decade
# CL3 Used in Last Year
# CL4 Used in Last Month 
# CL5 Used in Last Week 
# CL6 Used in Last Day

raw_data_df <- rename(raw_data_df,
       alcohol = X14,
       amphet = X15,
       amyl = X16,
       benzos = X17,
       caff = X18,
       cannabis = X19,
       choc = X20,
       coke = X21,
       crack = X22,
       ecstasy = X23,
       heroin = X24,
       ketamine = X25,
       legalh = X26,
       lsd = X27,
       meth = X28,
       mushrooms = X29,
       nicotine = X30,
       semer = X31,
       vsa = X32)

raw_data_df <- mutate(raw_data_df, across(alcohol:vsa, ~{
  case_when(. == 'CL0' ~ 'Never Used',
            . == 'CL1' ~ 'Used over a Decade Ago',
            . == 'CL2' ~ 'Used in Last Decade',
            . == 'CL3' ~ 'Used in Last Year',
            . == 'CL4' ~ 'Used in Last Month',
            . == 'CL5' ~ 'Used in Last Week',
            . == 'CL6' ~ 'Used in Last Day')})
) %>% mutate(across(alcohol:vsa, 
                    ~factor(., 
                            levels = c("Never Used",
                                       "Used over a Decade Ago",
                                       "Used in Last Decade",
                                       "Used in Last Year",
                                       "Used in Last Month",
                                       "Used in Last Week",
                                       "Used in Last Day"),
                            ordered = TRUE
                    )
))

# Rename variables --------------------------------------------------------

raw_data_df <- rename(raw_data_df,
       nscore = neuroticism,
       escore = extraversion,
       oscore = openness,
       ascore = agreeableness,
       cscore = conscientiousness)

# Convert some variables to factors ---------------------------------------

raw_data_df <- mutate(raw_data_df,
                      across(c(id, gender, country, ethnicity),
                             as.factor)
)

# Some variables are ordered factors
raw_data_df <- mutate(raw_data_df,
                      education = factor(education,
                                         levels = education_info_df %>% 
                                                  select(education) %>% 
                                                  distinct() %>% 
                                                  pull(),
                                         ordered = TRUE
                      ),
                      age = factor(age,
                                   levels = age_info_df %>% 
                                            select(age) %>% 
                                            distinct() %>% 
                                            pull(),
                                   ordered = TRUE
                      )
)

# Add to package data -----------------------------------------------------

drugriskfactors <- raw_data_df
usethis::use_data(drugriskfactors, overwrite = TRUE)
