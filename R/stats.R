#' Format a p-value for reporting
#'
#' Converts a numeric p-value into a character string suitable for manuscripts
#' or console output.
#' Values smaller than a chosen threshold are printed as “\code{p < eps}”;
#' otherwise the value is shown as “\code{p = …}” with a fixed number of
#' significant digits and *without* scientific notation.
#'
#' @param p A numeric scalar representing the p-value to format.
#' @param eps Numeric threshold.
#'   If \code{p < eps} the string \code{"p < eps"} is returned.
#'   Defaults to \code{0.001}.
#' @param digits Integer giving the number of significant digits to keep when
#'   \code{p >= eps}.
#'   Passed to \code{\link[base]{format}()}.
#'   Defaults to \code{3}.
#'
#' @return A length-one character vector containing the formatted p-value.
#'
#' @examples
#' format_pval(0.0789)
#' format_pval(2e-05)
#' format_pval(0.00456, eps = 0.01, digits = 2)
#'
#' @export
format_pval <- function(p, eps = 0.001, digits = 3) {
  if (p < eps) {
    sprintf("p < %s", eps)
  } else {
    sprintf("p = %s", format(p, scientific = FALSE, digits = digits))
  }
}
