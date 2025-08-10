## code to prepare `volunteering` dataset

volunteering <- tibble::as_tibble(carData::Cowles)
usethis::use_data(volunteering, overwrite = TRUE)
