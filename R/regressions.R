#' Visual diagnostics for linear models: residuals, Q–Q, scale–location, leverage, and histogram
#'
#' @description
#' Produce one of five standard diagnostic plots for a fitted \code{lm} model
#' using \pkg{ggplot2} layers and \pkg{broom}’s augmented data.
#' These plots help check the core assumptions of the normal linear model:
#' a linear conditional mean, errors centred at zero, roughly constant variance,
#' and error distributions close enough to normal for small‑sample \eqn{t}-based inference.
#'
#' @details
#' The function draws one of five plots, selected via \code{type}.
#'
#' \strong{1) \code{type = "resid"} — Residuals vs fitted}
#'
#' Raw residuals \eqn{\hat\varepsilon_i = y_i - \hat y_i} versus fitted values \eqn{\hat y_i}.
#' A well‑specified model shows a roughly horizontal band around 0 with no systematic pattern.
#'
#' \emph{What to look for}
#' \itemize{
#'   \item \emph{Linearity:} Curvature suggests the conditional mean is not linear in \eqn{x}.
#'   \item \emph{Constant variance:} Funnel shapes indicate heteroskedasticity.
#'   \item \emph{Outliers / structure:} Large vertical distances or clustered stripes merit investigation.
#' }
#'
#' \strong{2) \code{type = "qq"} — Normal Q–Q of standardised residuals}
#'
#' Ordered standardised residuals against theoretical normal quantiles with a reference line.
#' Points close to the line indicate approximate normality; tail bending indicates heavy tails; an S‑shape indicates skewness.
#'
#' \strong{3) \code{type = "scale"} — Scale–location}
#'
#' Plots \eqn{\sqrt{|r_i|}}{sqrt(|r_i|)} versus fitted values, where \eqn{r_i} are standardised residuals.
#' A flat band supports constant variance; upward or downward trends suggest heteroskedasticity.
#'
#' \strong{4) \code{type = "lev"} — Standardised residuals vs leverage}
#'
#' Standardised residuals (\code{.std.resid}) versus leverage (\code{.hat}).
#' A vertical reference line is drawn at \eqn{2p/n}, where \eqn{p} is the number of coefficients
#' (including the intercept) and \eqn{n} is the sample size.
#' Horizontal lines at \eqn{\pm 2}{+/- 2} mark unusually large residuals.
#' Point size encodes Cook’s distance (\code{.cooksd}) to cue overall influence.
#'
#' \emph{Reading the plot}
#' \itemize{
#'   \item Right of \eqn{2p/n} with small residuals: high leverage but not influential.
#'   \item Right of \eqn{2p/n} and beyond \eqn{|2|}{2}: candidates for strong influence.
#' }
#'
#' \strong{5) \code{type = "hist"} — Histogram of residuals with normal overlay}
#'
#' Histogram of raw residuals on a density scale, overlaid with the normal density
#' \eqn{N(0, \sigma^2)} where \eqn{\sigma} is the residual standard deviation
#' (\code{sigma(model)}). A symmetric, bell‑shaped histogram centred at 0 that
#' roughly follows the red curve supports the normal‑error assumption; visible skew
#' or heavier‑than‑normal tails point to departures that may affect small‑sample inference.
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
#'     \item \code{"lev"} — standardised residuals vs leverage, with reference lines; point size indicates Cook’s distance.
#'     \item \code{"hist"} — histogram of residuals with normal density overlay.
#'   }
#' @param alpha Point transparency for scatter plots (default \code{0.7}).
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
#' lm_diagnostic_plot(fit, "hist")
#' @export
lm_diagnostic_plot <- function(model,
                               type = c("resid", "qq", "scale", "lev", "hist"),
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

  if (type == "hist") {
    s <- stats::sigma(model) # residual standard deviation
    return(
      ggplot2::ggplot(aug, ggplot2::aes(.resid)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
          bins = 20, colour = "white"
        ) +
        ggplot2::stat_function(
          fun = stats::dnorm,
          args = list(mean = 0, sd = s),
          colour = "red", linewidth = 0.8
        ) +
        ggplot2::geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
        ggplot2::labs(x = "Residual", y = "Density")
    )
  }
}



#' Bootstrap CIs for residual skewness and kurtosis
#'
#' Computes bootstrap percentile confidence intervals for the skewness
#' and kurtosis of the residuals from an \code{lm} model.
#'
#' @param model A fitted \code{lm} object.
#' @param reps  Number of bootstrap resamples. Default \code{10000}.
#' @param level Confidence level. Default \code{0.95}.
#' @param seed  Optional integer seed for reproducibility. Default \code{NULL}.
#'
#' @return A tibble with two rows and columns:
#' \code{measure} (\code{"skewness"} or \code{"kurtosis"}),
#' \code{estimate}, \code{lower}, \code{upper}, \code{n}, \code{reps}, \code{level}.
#'
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' residual_shape_ci(fit, reps = 2000, seed = 1)
#' @export
residual_shape_ci <- function(model, reps = 10000, level = 0.95, seed = NULL) {
  stopifnot(inherits(model, "lm"))
  if (!is.null(seed)) set.seed(seed)

  # residuals
  resids <- broom::augment(model)$.resid
  resids <- resids[is.finite(resids)]
  n <- length(resids)
  if (n < 5) stop("Not enough finite residuals to compute moments.")

  df_res <- tibble::tibble(.resid = resids)

  skew_obs <- skewness(resids)
  kurt_obs <- kurtosis(resids)

  # bootstrap distributions using infer
  boot <- df_res |>
    infer::specify(response = .resid) |>
    infer::generate(reps = reps, type = "bootstrap")

  skew_boot <- dplyr::summarise(boot, stat = skewness(.resid))
  kurt_boot <- dplyr::summarise(boot, stat = kurtosis(.resid))

  # percentile CIs
  skew_ci <- infer::get_confidence_interval(skew_boot, level = level, type = "percentile")
  kurt_ci <- infer::get_confidence_interval(kurt_boot, level = level, type = "percentile")

  tibble::tibble(
    measure  = c("skewness", "kurtosis"),
    estimate = c(skew_obs, kurt_obs),
    lower    = c(skew_ci$lower_ci, kurt_ci$lower_ci),
    upper    = c(skew_ci$upper_ci, kurt_ci$upper_ci)
  )
}


#' Bootstrap percentile CIs for all lm coefficients and sigma using infer
#'
#' @param model A fitted \code{lm} object.
#' @param reps  Number of bootstrap resamples (default 5000).
#' @param level Confidence level (default 0.95).
#' @param seed  Optional integer for reproducibility.
#' @return A tibble with rows for each coefficient and for \code{sigma},
#'   columns: \code{term}, \code{estimate}, \code{lower}, \code{upper}.
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' lm_bootstrap_ci(fit, reps = 2000, seed = 1)
#' @export
lm_bootstrap_ci <- function(model, reps = 5000, level = 0.95, seed = NULL) {
  stopifnot(inherits(model, "lm"))
  if (!is.null(seed)) set.seed(seed)

  df <- model.frame(model)
  form <- stats::formula(model)

  # observed estimates
  beta_hat <- stats::coef(model)
  sigma_hat <- stats::sigma(model)

  # generate bootstrap resamples with infer (pairs bootstrap)
  boots <- df |>
    infer::specify(formula = form) |>
    infer::generate(reps = reps, type = "bootstrap")

  # fit the model in each replicate and collect coefficients and sigma
  coef_boot <- boots |>
    dplyr::group_by(replicate) |>
    dplyr::group_modify(function(.x, .y) {
      fit <- stats::lm(form, data = .x)
      tibble::tibble(
        term     = names(stats::coef(fit)),
        estimate = unname(stats::coef(fit)),
        sigma    = stats::sigma(fit)
      )
    }) |>
    dplyr::ungroup()

  # coefficient CIs (percentile)
  alpha <- 1 - level
  coef_ci <- coef_boot |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      lower = stats::quantile(estimate, probs = alpha / 2, names = FALSE, type = 7),
      upper = stats::quantile(estimate, probs = 1 - alpha / 2, names = FALSE, type = 7),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      estimate = unname(beta_hat[term])
    )

  # sigma CI (summarise once per replicate, then percentile)
  sigma_ci <- coef_boot |>
    dplyr::distinct(replicate, sigma) |>
    dplyr::summarise(
      lower = stats::quantile(sigma, probs = alpha / 2, names = FALSE, type = 7),
      upper = stats::quantile(sigma, probs = 1 - alpha / 2, names = FALSE, type = 7),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      term = "sigma",
      estimate = sigma_hat,
      .before = 1
    )

  dplyr::bind_rows(coef_ci, sigma_ci) |>
    dplyr::relocate(term, estimate, lower, upper)
}
