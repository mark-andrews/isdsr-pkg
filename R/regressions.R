#' Visual diagnostics for linear models: residuals, Q–Q, scale–location, and leverage
#'
#' @description
#' Produce one of four standard diagnostic plots for a fitted \code{lm} model
#' using \pkg{ggplot2} layers and \pkg{broom}’s augmented data.
#' These plots help check the core assumptions of the normal linear model:
#' a linear conditional mean, errors centred at zero, roughly constant variance,
#' and error distributions close enough to normal for small‑sample \eqn{t}-based inference.
#'
#' @details
#' The function draws one of four plots, selected via \code{type}.
#'
#' \strong{1) \code{type = "resid"} — Residuals vs fitted}
#'
#' Raw residuals \eqn{\hat\varepsilon_i = y_i - \hat y_i} versus fitted values \eqn{\hat y_i}.
#' A well‑specified model shows a roughly horizontal band around 0 with no systematic pattern.
#'
#' \emph{What to look for}
#' \itemize{
#'   \item \emph{Linearity:} Curvature (e.g., U‑shape) suggests the conditional mean is not linear in \eqn{x}.
#'         Consider transforming a variable or changing the mean function.
#'   \item \emph{Constant variance:} A funnel pattern—residual spread growing or shrinking with the fitted values—
#'         indicates heteroskedasticity. Consequences include misleading standard errors and p‑values.
#'         Remedies include variance‑stabilising transforms (e.g., logs), modelling the variance,
#'         or reporting heteroskedasticity‑robust standard errors (which do not change \eqn{\hat\beta}, only the uncertainty).
#'   \item \emph{Outliers:} Points far from 0 vertically (large residuals) may indicate outliers or data issues.
#'   \item \emph{Structure / clustering:} Stripes or clusters can signal omitted groups, time dependence,
#'         or different variances by subgroup.
#' }
#'
#' \strong{2) \code{type = "qq"} — Normal Q–Q of standardised residuals}
#'
#' Ordered standardised residuals against theoretical normal quantiles with a reference line.
#' Points should lie close to the line for approximate normality of errors.
#' Departures in both tails indicate heavy tails; an S‑shape indicates skewness.
#'
#' \emph{What to look for}
#' \itemize{
#'   \item \emph{Heavy tails:} Points bending away from the line at both ends
#'         (above in the upper tail, below in the lower tail) imply heavier tails than normal.
#'   \item \emph{Skewness:} A systematic S‑shape (one tail above, the other below) indicates skew.
#'         Transformations of the outcome may help.
#'   \item \emph{Isolated extremes:} A few points far from the line are candidate outliers; investigate their influence.
#' }
#'
#' \strong{3) \code{type = "scale"} — Scale–location}
#'
#' Plots \eqn{\sqrt{|r_i|}}{sqrt(|r_i|)} versus fitted values, where \eqn{r_i} are standardised residuals.
#' This stabilises the spread and makes trends in residual variability easier to see.
#' A flat band supports constant variance; upward or downward trends suggest heteroskedasticity.
#'
#' \emph{What to look for}
#' \itemize{
#'   \item \emph{Flat band:} Supports the constant‑variance assumption.
#'   \item \emph{Upward trend:} Variance increases with the mean—intervals may be too narrow at large fitted values.
#'   \item \emph{Downward trend:} Variance decreases with the mean—intervals may be too wide at small fitted values.
#' }
#'
#' \strong{4) \code{type = "lev"} — Standardised residuals vs leverage}
#'
#' Standardised residuals (\code{.std.resid}) versus leverage (\code{.hat}).
#' Points far to the right have high leverage (unusual \eqn{x} values).
#' Points far up or down have large residuals.
#' Large influence typically arises when \emph{both} leverage and residual are large.
#' A vertical reference line is drawn at \eqn{2p/n}, where \eqn{p} is the number of
#' model coefficients (including the intercept) and \eqn{n} is the sample size.
#' Horizontal lines at \eqn{\pm 2}{+/- 2} mark unusually large residuals.
#' Point size encodes Cook’s distance (\code{.cooksd}) to cue overall influence.
#'
#' \emph{Reading the plot}
#' \itemize{
#'   \item Left band near 0: typical leverage; investigate residual structure with the other plots.
#'   \item Right of \eqn{2p/n} with small residuals: high leverage but not influential.
#'   \item Right of \eqn{2p/n} and beyond \eqn{|2|}{2}: candidates for strong influence.
#'         Check data quality and model form before taking action.
#' }
#'
#' \strong{General guidance}
#' \itemize{
#'   \item Normality mainly affects small‑sample accuracy of p‑values and intervals;
#'         linearity and constant variance affect bias and coverage at any size.
#'   \item Diagnostics prompt investigation; decisions should consider context and
#'         subject‑matter knowledge rather than fixed rules.
#' }
#'
#' @param model An \code{lm} object.
#' @param type  One of:
#'   \itemize{
#'     \item \code{"resid"} — residuals vs fitted (with a loess smooth).
#'     \item \code{"qq"} — Q–Q plot of standardised residuals with reference line.
#'     \item \code{"scale"} — scale–location plot \eqn{\sqrt{|r_i|}}{sqrt(|r_i|)} vs fitted.
#'     \item \code{"lev"} — standardised residuals vs leverage, with reference lines;
#'           point size indicates Cook’s distance.
#'   }
#' @param alpha Point transparency (default \code{0.7}).
#'
#' @return A \code{ggplot} object, which you can further customise.
#'
#' @seealso
#' \code{stats::plot.lm()} for the classic base‑R set (includes leverage and Cook’s contours);
#' \pkg{broom} \code{augment()} for the columns used here.
#'
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' lm_diagnostic_plot(fit, "resid")
#' lm_diagnostic_plot(fit, "qq")
#' lm_diagnostic_plot(fit, "scale")
#' lm_diagnostic_plot(fit, "lev")
#' @export
lm_diagnostic_plot <- function(model,
                               type = c("resid", "qq", "scale", "lev"),
                               alpha = 0.7) {
  stopifnot(inherits(model, "lm"))
  type <- match.arg(type)
  aug <- broom::augment(model)

  if (type == "resid") {
    return(
      ggplot2::ggplot(aug, ggplot2::aes(.fitted, .resid)) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
        ggplot2::geom_point(alpha = alpha) +
        ggplot2::geom_smooth(se = FALSE, colour = "red", linewidth = 0.5) +
        ggplot2::labs(x = "Fitted values", y = "Residuals")
    )
  }

  if (type == "qq") {
    return(
      ggplot2::ggplot(aug, ggplot2::aes(sample = .std.resid)) +
        ggplot2::stat_qq(alpha = alpha) +
        ggplot2::stat_qq_line(colour = "red", linewidth = 0.5) +
        ggplot2::labs(x = "Theoretical quantiles", y = "Standardised residuals")
    )
  }

  if (type == "scale") {
    return(
      ggplot2::ggplot(aug, ggplot2::aes(.fitted, sqrt(abs(.std.resid)))) +
        ggplot2::geom_point(alpha = alpha) +
        ggplot2::geom_smooth(se = FALSE, colour = "red", linewidth = 0.5) +
        ggplot2::labs(
          x = "Fitted values",
          y = expression(sqrt("|standardised residual|"))
        )
    )
  }

  if (type == "lev") {
    n <- nrow(aug)
    p <- length(stats::coef(model)) # includes intercept
    lev_thr <- 2 * p / n

    return(
      ggplot2::ggplot(
        aug,
        ggplot2::aes(x = .hat, y = .std.resid, size = sqrt(.cooksd))
      ) +
        ggplot2::geom_hline(
          yintercept = c(-2, 0, 2),
          colour = c("grey70", "grey50", "grey70"),
          linewidth = 0.3, linetype = c(2, 1, 2)
        ) +
        ggplot2::geom_vline(
          xintercept = lev_thr, colour = "red",
          linewidth = 0.5, linetype = 2
        ) +
        ggplot2::geom_point(alpha = alpha) +
        ggplot2::scale_size_continuous(range = c(1, 4), guide = "none") +
        ggplot2::labs(
          x = "Leverage (hat value)",
          y = "Standardised residual",
          caption = sprintf("Vertical line at 2p/n = %.3f", lev_thr)
        )
    )
  }
}
