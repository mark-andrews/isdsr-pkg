## Code to prepare `gss2024a` dataset
# -----------------------------------

# The sav file was in a zip named GSS_spss.zip obtained from
# https://gss.norc.org/us/en/gss/get-the-data/spss.html
#
# md5sum GSS_spss.zip
# 3b41a62390d6e5178d8b1e8d1cdd6469 GSS_spss.zip
#
# Contents ====================================
#
# unzip -l GSS_spss.zip
# Archive:  GSS_spss.zip
# Length      Date    Time    Name
# ---------  ---------- -----   ----
# 0  2025-05-22 17:04            GSS_spss/
# 360582  2025-05-22 13:37       GSS_spss/GSS 2024 - Whats New.pdf
# 2574973  2025-05-22 16:16      GSS_spss/GSS 2024 Codebook.pdf
# 156555  2025-05-22 13:38       GSS_spss/GSS 2024 Release Variables.pdf
# 3820460746  2025-05-20 19:49   GSS_spss/gss7224_r1.sav
# 323  2025-05-22 00:53          GSS_spss/ReadMe.txt
# 102929  2025-05-22 13:43       GSS_spss/Release Notes 7224.pdf
# ---------                     -------
# 3823656108                     7 files

# md5sum GSS_spss/gss7224_r1.sav
# 8ccea305cc8330533794fd7a8f7c2e26  GSS_spss/gss7224_r1.sav

# Neither the zip (87MB) nor the sav (3.6GB) are included in this package because they are too large.

library(tidyverse)

gss7224_r1 <- haven::read_sav("~/Downloads/GSS_spss/gss7224_r1.sav", user_na = F)

gss2021 <- gss7224_r1 |>
  filter(year == 2021) %>%
  select(income = realrinc, degree, sex, age, marital, hours = hrs1, wrkstat, prestige = prestg10, childs) %>%
  filter(!is.na(income), sex %in% 1:2, !is.na(degree)) %>%
  mutate(
    lincome = log10(income),
    sex = factor(sex, labels = c("Male", "Female")),
    degree = factor(degree, labels = c("<HS", "HS", "JrColl", "BA", "Grad")),
    marital = haven::as_factor(marital, levels = "labels"),
    fulltime = factor(ifelse(wrkstat == 1, "Full-time", "Other"))
  ) |>
  select(-wrkstat) |>
  relocate(lincome, .after = income) |>
  mutate(across(
    where(haven::is.labelled), # <-- selects every dbl+lbl column
    as.numeric # strips the “labelled” class, keeps the numbers
  )) |>
  # drop empty levels; particularly `marital`
  droplevels()


usethis::use_data(gss2021, overwrite = TRUE)
