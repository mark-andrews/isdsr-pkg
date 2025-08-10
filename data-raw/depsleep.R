## data-raw/depsleep.R

# keep imports minimal in data-raw scripts
depsleep <- NHANES::NHANES |>
  dplyr::filter(Age >= 18) |>
  dplyr::transmute(
    Depressed = factor(Depressed,
      levels = c("None", "Several", "Most"),
      ordered = TRUE
    ),
    SleepHrsNight = SleepHrsNight,
    # keep only the two categories you want; others become NA then dropped
    Work = factor(Work, levels = c("NotWorking", "Working"))
  ) |>
  dplyr::filter(!is.na(Depressed), !is.na(Work))
# optionally also drop missing sleep:
# |> dplyr::filter(!is.na(SleepHrsNight))

usethis::use_data(depsleep, overwrite = TRUE)
