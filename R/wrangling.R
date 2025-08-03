#' Format Numeric Columns to Fixed Digits
#'
#' This function formats specified numeric columns in a data frame to a fixed number of decimal places.
#'
#' @details
#' Tibble data frames display numeric values to a certain number of significant
#' figures, determined by the `pillar.sigfig` option. Sometimes it
#' may be useful or necessary to see values to a fixed number of digits. This
#' can be accomplished with \link[tibble]{num}. This function is a convenience function that applies
#' \link[tibble]{num} to all, or a specified subset, of the numeric vectors in a
#' tibble.
#'
#' @param data A data frame or tibble containing the columns to format.
#' @param ... <[`tidy-select`][dplyr::select]> Columns to apply the fixed digit formatting to.
#' If no columns are specified, all numeric columns are selected.
#' @param .digits An integer specifying the number of decimal places to format to.
#' Default is 3.
#'
#' @return A data frame with the selected numeric columns formatted to the specified number of decimal places.
#'
#' @examples
#' # Format all numeric columns to 3 decimal places
#' mtcars_df <- tibble::as_tibble(mtcars)
#' to_fixed_digits(mtcars_df)
#'
#' # Format columns mpg to qsec to 3 decimal places
#' to_fixed_digits(mtcars_df, mpg:qsec)
#'
#' # Format specific columns to 2 decimal places
#' to_fixed_digits(mtcars_df, mpg, hp, .digits = 2)
#'
#' @export
to_fixed_digits <- function(data, ..., .digits = 3) {
  dots <- rlang::enquos(...)
  as_digits <- function(x) tibble::num(x, digits = .digits)

  if (rlang::is_empty(dots)) {
    # Use all columns if no `...` are supplied
    dplyr::mutate(data, dplyr::across(dplyr::where(is.numeric), as_digits))
  } else {
    dplyr::mutate(data, dplyr::across(c(!!!dots) & dplyr::where(is.numeric), as_digits))
  }
}

#' Z-score selected numeric columns
#'
#' @param data  A data frame / tibble.
#' @param ...   Tidy-select expression(s) that pick the columns to rescale.
#'              If omitted, *all* numeric columns are standardised.
#'
#' @return A data frame of the same class as `data`.
#' @examples
#' # Format all numeric columns to 3 decimal places
#' mtcars_df <- tibble::as_tibble(mtcars)
#' re_scale(mtcars_df)
#'
#' # Just selected columns
#' re_scale(mtcars_df, mpg:qsec)
#'
#' @export
re_scale <- function(data, ...) {
  dots <- rlang::enquos(...)

  z <- function(x) {
    s <- stats::sd(x, na.rm = TRUE)
    if (is.finite(s) && s > 0) (x - mean(x, na.rm = TRUE)) / s else x
  }

  if (rlang::is_empty(dots)) {
    dplyr::mutate(
      data,
      dplyr::across(dplyr::where(is.numeric), z)
    )
  } else {
    dplyr::mutate(
      data,
      dplyr::across(c(!!!dots) & dplyr::where(is.numeric), z)
    )
  }
}


#' Compact summary of key model statistics
#'
#' Extracts a reduced set of headline measures from a fitted `lm` object—
#' the model \eqn{R^{2}} and adjusted \eqn{R^{2}}, the overall
#' \eqn{F}-statistic, its numerator and denominator degrees of freedom,
#' and the corresponding *p*-value—omitting AIC, BIC, deviance, and other
#' information that `broom::glance()` ordinarily returns.
#'
#' @param model An object that inherits from class `"lm"`.
#'
#' @return A one-row tibble with columns
#'   \describe{
#'     \item{`r_sq`}{\eqn{R^{2}}.}
#'     \item{`adj_r_sq`}{Adjusted \eqn{R^{2}}.}
#'     \item{`fstat`}{Model \eqn{F}-statistic.}
#'     \item{`num_df`}{Numerator degrees of freedom.}
#'     \item{`den_df`}{Denominator (residual) degrees of freedom.}
#'     \item{`p_value`}{Two-sided *p*-value for the overall \eqn{F} test.}
#'   }
#'
#' @examples
#' fit <- lm(mpg ~ disp + hp, data = mtcars)
#' lm_model_summary(fit)
#'
#' @importFrom broom glance
#' @importFrom dplyr select
#' @export
lm_model_summary <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class \"lm\".", call. = FALSE)
  }

  broom::glance(model) |>
    dplyr::select(
      r_sq     = r.squared,
      adj_r_sq = adj.r.squared,
      fstat    = statistic,
      num_df   = df,
      den_df   = df.residual,
      p_value  = p.value
    )
}
