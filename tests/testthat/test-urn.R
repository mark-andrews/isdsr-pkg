`%>%` <- magrittr::`%>%`

test_that("urn_sampler returns correct number of rows", {
  expect_equal(nrow(urn_sampler()), 1)
  expect_equal(nrow(urn_sampler(repetitions = 10)), 10)
  expect_equal(nrow(urn_sampler(repetitions = 25, replace = F)), 25)
})

test_that("urn_sampler has correct sample size on each row", {
  expect_equal(urn_sampler(repetitions = 10, sample_size = 50) %>%
                  dplyr::mutate(n = red + black) %>% 
                  dplyr::pull(n) %>% 
                  unique(), 
                50)
  expect_equal(urn_sampler(repetitions = 1,
                           replace = F,
                           sample_size = 35) %>%
                 dplyr::mutate(n = red + black) %>% 
                 dplyr::pull(n) %>% 
                 unique(), 
               35)
})