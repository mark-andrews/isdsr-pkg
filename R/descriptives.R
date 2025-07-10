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


#' Inter-percentile range
#'
#' Computes the distance between the \eqn{p}-th and \eqn{(1 - p)}-th sample
#' percentiles.  `ipr()` is the general work-horse; `idr()` (inter-decile
#' range) and `itr()` (inter-tercile range) are thin wrappers that set
#' \eqn{p = 0.10} and \eqn{p = 1/3}, respectively.
#'
#' @param x      Numeric vector.
#' @param p      A proportion between 0 and 0.5 (exclusive).
#'               Default `0.25` reproduces the inter-quartile range.
#' @param na.rm  Logical; if `TRUE`, remove `NA`s before computing.
#' @param type   Passed to [stats::quantile()].  Default `7` matches `IQR()`.
#' @param ...    Additional arguments passed to [stats::quantile()].
#'
#' @return A single numeric value: the difference between
#'   \eqn{Q_{1 - p}} and \eqn{Q_{p}}. Returns `NA_real_` if the range is
#'   undefined (e.g. all observations missing).
#'
#' @examples
#' x <- rnorm(100)
#' ipr(x) # same idea as IQR, but p = 0.25 by default
#' idr(x) # 10th–90th percentile range
#' itr(x) # 33.3rd–66.7th percentile range
#'
#' @export
ipr <- function(x, p = 0.25, na.rm = TRUE, type = 7, ...) {
  stopifnot(
    is.numeric(x),
    is.numeric(p), length(p) == 1L,
    p > 0, p < 0.5
  )

  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }

  qs <- stats::quantile(x, probs = c(p, 1 - p), type = type, ...) |> unname()
  diff(qs)
}

#' @describeIn ipr Inter-decile range (p = 0.10)
#' @export
idr <- function(x, na.rm = FALSE, type = 7, ...) {
  ipr(x, p = 0.10, na.rm = na.rm, type = type, ...)
}

#' @describeIn ipr Inter-tercile range (p = 1/3)
#' @export
itr <- function(x, na.rm = FALSE, type = 7, ...) {
  ipr(x, p = 1 / 3, na.rm = na.rm, type = type, ...)
}


#' Sample kurtosis (raw by default)
#'
#' Calculates the sample \emph{raw} kurtosis of a numeric vector using
#' [moments::kurtosis()].  If you want \emph{excess} kurtosis
#' (raw – 3) set `excess = TRUE`.
#'
#' @param x      Numeric vector.
#' @param na.rm  Logical; if `TRUE` (default) remove `NA` values
#'   before computing the statistic.
#' @param excess Logical; if `TRUE` subtract 3 so the normal
#'   distribution maps to 0.  Defaults to `FALSE` (raw kurtosis).
#'
#' @return A single numeric value: raw kurtosis by default, excess
#'   kurtosis when `excess = TRUE`.  Returns `NA_real_` if the vector
#'   has fewer than four non‐missing observations.
#'
#' @details
#' *Raw* kurtosis equals 3 for a normal distribution.
#' *Excess* kurtosis is raw – 3, so the normal benchmark is 0.
#'
#' | Excess value | Tail description | Raw value |
#' |--------------|------------------|-----------|
#' | > 0          | heavy‐tailed     | > 3       |
#' | = 0          | normal‐tailed    | = 3       |
#' | < 0          | light‐tailed     | < 3       |
#'
#' @examples
#' library(dplyr)
#'
#' # Raw kurtosis (normal ≈ 3)
#' summarise(whr2025,
#'   raw_kurt_happiness = kurtosis(happiness),
#'   raw_kurt_gdp       = kurtosis(gdp)
#' )
#'
#' # Excess kurtosis (normal ≈ 0)
#' summarise(whr2025,
#'   excess_kurt_happiness = kurtosis(happiness, excess = TRUE),
#'   excess_kurt_gdp       = kurtosis(gdp, excess = TRUE)
#' )
#'
#' @export
kurtosis <- function(x, na.rm = TRUE, excess = FALSE) {
  k_raw <- moments::kurtosis(x, na.rm = na.rm)
  if (is.na(k_raw)) {
    return(k_raw)
  }
  if (excess) k_raw - 3 else k_raw
}
