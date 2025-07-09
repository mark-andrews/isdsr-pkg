#' Find the mode of a discrete (categorical or integer-like) variable
#'
#' `get_mode()` returns the most frequent value(s) of a variable in a data
#' frame.  If several values are tied for highest frequency, they are all
#' returned.  Missing values (`NA`) are removed before counting.
#'
#' @param data A data frame.
#' @param var  A column of `data`.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{`var`}{the modal value(s) of the `var`}
#'   \item{`count`}{how many times each mode occurs}
#' }
#'
#' @examples
#' ## Mode of the three-level income factor
#' get_mode(whr2025, income)
#'
#' @export
get_mode <- function(data, var) {
  data |>
    dplyr::filter(!is.na({{ var }})) |>
    dplyr::count({{ var }}, name = "count") |>
    dplyr::slice_max(count)
}

#' Approximate the mode of a continuous variable by binning
#'
#' `get_binned_mode()` divides a numeric variable into equal-width bins
#' (default = 10), counts the observations in each bin, and returns the
#' midpoint of the fullest bin.  This illustrates why the mode is usually
#' unhelpful for truly continuous data: the result depends entirely on how
#' you choose the bins.
#'
#' @param data   A data frame.
#' @param var    A numeric column of the data frame.
#' @param n_bins Integer, number of equal-width bins to create.  Defaults to 10.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{`var`}{the midpoint of the bin with the highest count;}
#'   \item{`count`}{the number of observations in that bin.}
#' }
#'
#' @examples
#' ## Approximate mode of the happiness scores (0–10 scale)
#' get_binned_mode(whr2025, happiness, n_bins = 5)
#'
#' ## same idea applied to raw GDP (skewed, wide-range data)
#' get_binned_mode(whr2025, gdp, n_bins = 20)
#'
#' @export
get_binned_mode <- function(data, var, n_bins = 10) {
  data |>
    dplyr::filter(!is.na({{ var }})) |>
    dplyr::mutate(
      .bin = cut({{ var }},
        breaks = n_bins,
        include.lowest = TRUE,
        labels = FALSE
      )
    ) |>
    dplyr::summarise(
      {{ var }} := mean({{ var }}, na.rm = TRUE),
      bin_count = dplyr::n(),
      .by = .bin
    ) |>
    dplyr::slice_max(bin_count) |>
    dplyr::select({{ var }}, count = bin_count)
}


#' Range width (max minus min)
#'
#' Computes the distance between the smallest and largest finite values in a
#' numeric vector.  If the vector is empty, contains only `NA`s (and
#' `na.rm = TRUE`), or contains only `Inf`/`-Inf`, the function returns
#' `NA_real_`.
#'
#' @param x      Numeric vector.
#' @param na.rm  Logical; if `TRUE` (default) remove `NA` values before
#'   computing the range.
#'
#' @return A single numeric value `max(x) - min(x)`, or `NA_real_` when the
#'   range is undefined.
#'
#' @examples
#' get_range(c(3, 10, 7, 4)) # 7
#' get_range(42) # 0
#' get_range(c(2, NA, 8)) # 6
#' get_range(c(2, NA, 8), na.rm = FALSE) # NA
#' get_range(numeric(0)) # NA
#' get_range(c(Inf, -Inf)) # NA
#'
#' @export
get_range <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]

  if (length(x) == 0L ||
    all(is.infinite(x))) {
    return(NA_real_)
  }

  max(x) - min(x)
}
