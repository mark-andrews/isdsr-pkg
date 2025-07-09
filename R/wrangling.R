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
