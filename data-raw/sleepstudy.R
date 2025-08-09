# sleepstudy is exactly the dataset of the same name from lme4
# but converted to a tibble

data("sleepstudy", package = "lme4")

sleepstudy <- sleepstudy |> tibble::as_tibble()

usethis::use_data(sleepstudy, overwrite = TRUE)
