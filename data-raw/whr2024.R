## Code to prepare `whr2024` dataset

# ==== Description / background ======
# The `WHR24_Statistical_Appendix_figures_68-91.txt` contains the text of the
# figures 68 to 91 in WHR24_Statistical_Appendix.pdf, obtained at
# https://files.worldhappiness.report/WHR24_Statistical_Appendix.pdf
# following links from https://www.worldhappiness.report/ed/2024/#appendices-and-data
# Obtained on 29 July 2025.
#
# The pdf to text was dumped by `pdftotext -layout
# WHR24_Statistical_Appendix.pdf` to give `WHR24_Statistical_Appendix.txt`.
#
# These figures provide the raw values of some predictor variables that predict
# the countries happiness.
#
# The text from these figures looks like this:
#
# Natural Log of Per-Capita GDP
#
#     1. Luxembourg(115,041)
#     2. Ireland(111,256)
#     3. Singapore(107,891)
#     4. United Arab Emirates( 73,318)
#     5. Switzerland( 70,670)
#     6. Norway( 67,061)
#     7. United States( 64,595)
#     8. Hong Kong S.A.R. of China( 59,254)
#
# For this variable, by the way, the numbers in brackets are the original
# per capita GDP, not the logs. The rest of the figure shows the the log
#
# The code below extracts the values for each country for each variable.

library(tidyverse)

# ---------- 0. read the cleaned plain‑text dump ------------------------------
txt <- readLines("data-raw/raw_data_files/WHR24_Statistical_Appendix_figures_68-91.txt", warn = FALSE, encoding = "UTF-8")

# ---------- 1. map headings → slug names ------------------------------------
anchors <- c(
  "natural log of" = "gdp", # it is not the log, see above
  "social support" = "social_support",
  "healthy life expectancy" = "healthy_life_exp",
  "freedom to make life choices" = "freedom",
  "generosity" = "generosity",
  "perceptions of corruption" = "corruption",
  "positive affect" = "positive_affect",
  "negative affect" = "negative_affect"
)
slugs <- unname(anchors) # handy later

# ---------- 2. helper: extract one "Country(value)" pair ---------------------
extract_pair <- function(line) {
  # grab the *last* (...) group on the line
  m <- regexec("\\(([^()]+)\\)\\s*$", line)
  bits <- regmatches(line, m)[[1]]
  if (length(bits) != 2) {
    return(NULL)
  } # no trailing (...) → skip

  value <- gsub("[ ,]", "", bits[2]) # drop spaces & commas, keep decimals
  value <- as.numeric(value)

  # remove rank prefix and the trailing "(value)" we just captured
  country <- sub("^\\s*\\d+\\.\\s*", "", line)
  country <- sub("\\s*\\([^()]*\\)\\s*$", "", country)
  country <- trimws(country)

  data.frame(Country = country, Value = value, stringsAsFactors = FALSE)
}

# ---------- 3. main loop -----------------------------------------------------
out <- setNames(vector("list", length(slugs)), slugs)
current <- NULL

for (ln in txt) {
  lo <- tolower(ln)

  # (a) –– does this line *contain* a variable heading?
  hits <- names(anchors)[vapply(names(anchors), grepl, logical(1), lo, fixed = FALSE)]
  if (length(hits)) {
    current <- anchors[hits[1]] # keep the first hit only
    next
  }

  # (b) –– if inside a section, harvest any country‑value rows
  if (!is.null(current) && grepl("\\([ 0-9.,]+\\)", ln)) {
    pr <- extract_pair(ln)
    if (!is.null(pr)) {
      out[[current]] <- if (is.null(out[[current]])) pr else rbind(out[[current]], pr)
    }
  }
}

# pivot -------------------------------------------------------------------

predictors <- bind_rows(out, .id = "variable") |>
  as_tibble() |>
  pivot_wider(names_from = variable, values_from = Value) |>
  rename(
    country = Country,
    support = social_support,
    hle = healthy_life_exp,
    positive = positive_affect,
    negative = negative_affect
  ) |>
  mutate(lgdp = log10(gdp), .after = gdp) |>
  # Let's leave out positive/negative.
  # We want to keep the dataset relatively small.
  select(-positive, -negative)

# join with data providing happiness score
whr2024 <- readxl::read_excel("data-raw/raw_data_files/WHR24_Data_Figure_2.1.xls") |>
  select(country = `Country name`, happiness = `Ladder score`) |>
  left_join(predictors, by = "country") |>
  drop_na()



# Write it ----------------------------------------------------------------

usethis::use_data(whr2024, overwrite = TRUE)
