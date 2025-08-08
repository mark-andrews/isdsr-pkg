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
#' @param label Logical. If \code{TRUE} and \code{type == "lev"}, label
#'   influential points (Cook’s distance > 4/n).
#'   Requires \pkg{ggrepel}. Ignored for other plot types.
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
                               alpha = 0.7,
                               label = TRUE) {
  stopifnot(inherits(model, "lm"))
  type <- match.arg(type)
  aug <- broom::augment(model)

  # row labels for possible use in the leverage plot
  aug$.rowname <- rownames(model.frame(model))
  if (is.null(aug$.rowname)) aug$.rowname <- seq_len(nrow(aug))

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

    base_plot <- ggplot2::ggplot(
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
        x       = "Leverage (hat value)",
        y       = "Standardised residual",
        caption = sprintf("Vertical line at 2p/n = %.3f", lev_thr)
      )

    if (label) {
      if (!requireNamespace("ggrepel", quietly = TRUE)) {
        warning("`label = TRUE` needs the ggrepel package; installing it is recommended.")
      } else {
        # choose influential points
        infl <- dplyr::filter(aug, .cooksd > 4 / n)
        if (nrow(infl) > 0) {
          base_plot <- base_plot +
            ggrepel::geom_text_repel(
              data = infl,
              ggplot2::aes(label = .rowname),
              size = 3,
              max.overlaps = Inf,
              box.padding = 0.4
            )
        }
      }
    }
    return(base_plot)
  }

  if (type == "hist") {
    s <- stats::sigma(model)
    return(
      ggplot2::ggplot(aug, ggplot2::aes(.resid)) +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ggplot2::after_stat(density)),
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
#' @param reps  Number of bootstrap resamples (default 10000).
#' @param level Confidence level (default 0.95).
#' @param sigma Should confidence intervals for residual standard deviation be included?
#' @param seed  Optional integer for reproducibility.
#' @return A tibble with rows for each coefficient and for \code{sigma},
#'   columns: \code{term}, \code{estimate}, \code{lower}, \code{upper}.
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' lm_bootstrap_ci(fit, reps = 2000, seed = 1)
#' @export
lm_bootstrap_ci <- function(model, reps = 10000, level = 0.95, sigma = FALSE, seed = NULL) {
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
    ) |>
    dplyr::arrange(match(term, names(beta_hat)))


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

  if (sigma) {
    results <- dplyr::bind_rows(coef_ci, sigma_ci)
  } else {
    results <- coef_ci
  }

  results |> dplyr::relocate(term, estimate, lower, upper)
}


#' Show treatment‑coded dummy columns for one categorical predictor
#'
#' @param data  A data frame or tibble.
#' @param var   A column name identifying a categorical variable.
#'
#' @return A tibble with the original factor column plus the \eqn{k-1} treatment
#'   dummies that `lm()` would use.  Each row is a unique level combination, so
#'   for a factor with \eqn{k} levels the tibble has \eqn{k} rows.
#'
#' @examples
#' get_dummy_code(pisa2022uk, gender)
#' get_dummy_code(iris, Species)
#'
#' @export
get_dummy_code <- function(data, var) {
  var_quo <- rlang::enquo(var)
  var_name <- rlang::as_name(var_quo)

  if (!var_name %in% names(data)) {
    stop("Variable `", var_name, "` not found in `data`.", call. = FALSE)
  }

  # Ensure the variable is a factor (so model_matrix uses contrasts)
  if (!is.factor(data[[var_name]])) {
    data <- dplyr::mutate(data, !!var_quo := factor(.data[[var_name]]))
  }

  # Build design matrix with modelr; drop intercept column
  mm <- modelr::model_matrix(data, stats::as.formula(paste0("~ ", var_name)))
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]

  # Combine and keep one row per level
  dplyr::bind_cols(
    dplyr::select(data, !!var_quo),
    tibble::as_tibble(mm)
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(!!var_quo)
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

#' R-squared with exact confidence interval
#'
#' Extracts the in-sample coefficient of determination (\eqn{R^{2}}) from a
#' fitted \code{\link[stats]{lm}} model and appends an exact two-sided
#' confidence interval obtained by inverting the non-central \(F\) distribution.
#'
#' Internally the function calls \code{\link[performance]{r2}} with
#' \code{ci_method = "analytical"}, which yields the Clopper–Pearson–type
#' limits.
#' The point estimate and limits are reshaped into a one-row tibble for
#' convenient downstream use.
#'
#' @param model An object of class \code{"lm"}.
#' @param level Confidence level to be passed on to
#'   \code{performance::r2()} (default \code{0.95}).
#'
#' @return A tibble with three numeric columns:
#'   \describe{
#'     \item{\code{rsq}}{the sample \eqn{R^{2}}}
#'     \item{\code{ci_low}}{lower confidence limit}
#'     \item{\code{ci_high}}{upper confidence limit}
#'   }
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' get_rsq(fit)
#'
#' @export
get_rsq <- function(model, level = 0.95) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class \"lm\".", call. = FALSE)
  }

  R2_stats <- performance::r2(model, ci = level, ci_method = "analytical")$R2

  R2_stats |>
    tibble::enframe() |>
    tidyr::pivot_wider(names_from = name, values_from = value) |>
    dplyr::select(rsq = R2, ci_low = CI_low, ci_high = CI_high)
}

#' Adjusted R-squared with exact confidence interval
#'
#' Retrieves the adjusted coefficient of determination (\eqn{R^{2}_{\text{adj}}})
#' from a fitted \code{\link[stats]{lm}} model and attaches a two-sided
#' confidence interval based on inversion of the non-central
#' \emph{F} distribution.
#'
#' The function is a light wrapper around
#' \code{\link[performance]{r2}(ci_method = "analytical")}, which computes
#' both the point estimate and an exact Clopper–Pearson-type interval for the
#' population value.
#' Results are returned as a one-row tibble for tidy downstream processing.
#'
#' @param model An object of class \code{"lm"}.
#' @param level Confidence level passed to \code{performance::r2()}
#'   (default \code{0.95}).
#'
#' @return A tibble with three numeric columns:
#'   \describe{
#'     \item{\code{adj_rsq}}{sample adjusted \eqn{R^{2}}}
#'     \item{\code{ci_low}}{lower confidence limit}
#'     \item{\code{ci_high}}{upper confidence limit}
#'   }
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' get_adjrsq(fit)
#' @export
get_adjrsq <- function(model, level = 0.95) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class \"lm\".", call. = FALSE)
  }

  adjR2_stats <- performance::r2(model,
    ci = level,
    ci_method = "analytical"
  )$R2_adjusted

  adjR2_stats |>
    tibble::enframe() |>
    tidyr::pivot_wider(names_from = name, values_from = value) |>
    dplyr::select(
      adj_rsq = `adjusted R2`,
      ci_low = CI_low,
      ci_high = CI_high
    )
}

#' Extract overall F statistic and p-value from a fitted linear model
#'
#' Returns the model-significance F statistic together with its p-value for an
#' object produced by \code{\link[stats]{lm}}.
#'
#' Extracts the overall
#' \eqn{F}-statistic, its numerator and denominator degrees of freedom,
#' and the corresponding *p*-value from `broom::glance()`.
#' A descriptive error is issued if the supplied object is not of class
#' \code{"lm"}.
#'
#' @inheritParams get_rsq
#'
#' @return A tibble with four columns:
#' \describe{
#'   \item{\code{fstat}}{the observed F statistic}
#'   \item{\code{num_df}}{numerator degrees of freedom}
#'   \item{\code{den_df}}{denominator degrees of freedom}
#'   \item{\code{p_value}}{upper-tail p-value for the test}
#' }
#'
#' @examples
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' get_fstat(mod)
#'
#' @export
get_fstat <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class \"lm\".", call. = FALSE)
  }

  broom::glance(model) |>
    dplyr::select(
      fstat    = statistic,
      num_df   = df,
      den_df   = df.residual,
      p_value  = p.value
    )
}

#' Format an lm() overall model null F-test for reporting
#'
#' Produce an APA-style string containing the F statistic,
#' its numerator and denominator degrees of freedom,
#' and the corresponding p-value.
#'
#' @param model An object that inherits from class \code{lm}.
#' @param digits A single integer giving the number of decimal places
#'   to show for the F statistic.
#'
#' @return A length-one character vector like
#'   \code{"F(4, 72) = 3.52, p = .011"}.
#' @export
#'
#' @examples
#' mod <- lm(mpg ~ factor(cyl), data = mtcars)
#' sprintf_fstat(mod)
sprintf_fstat <- function(model, digits = 2) {
  FSTAT <- get_fstat(model)
  df_1 <- round(FSTAT[["num_df"]])
  df_2 <- round(FSTAT[["den_df"]])
  fstat <- round(FSTAT[["fstat"]], digits)
  p_value <- format_pval(FSTAT[["p_value"]])

  sprintf("F(%d, %d) = %2.2f, %s", df_1, df_2, fstat, p_value)
}

#' Variance inflation factors (tidy output)
#'
#' Computes variance inflation factors (VIFs) for every term in an ordinary
#' least–squares model and returns the results as a two-column tibble.
#'
#' Internally the function calls \code{\link[car]{vif}} and tidies the numeric
#' vector it produces with \code{\link[tibble]{enframe}}.
#' A VIF measures how much the sampling variance of a regression coefficient is
#' inflated by collinearity among the predictors.
#' Rules of thumb often flag values above 5 or 10 as potential problems,
#' but appropriate cut-offs depend on sample size and study goals.
#'
#' @param model An object of class \code{"lm"}.
#'
#' @return A tibble with one row per model term and two numeric columns:
#'   \describe{
#'     \item{\code{term}}{the name of the predictor or interaction term}
#'     \item{\code{vif}}{the corresponding variance inflation factor}
#'   }
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
#' vif(fit)
#' @export
vif <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class \"lm\".", call. = FALSE)
  }

  car::vif(model) |>
    tibble::enframe(name = "term", value = "vif")
}


#' Partial correlation with tidyselect helpers
#'
#' Calculates the Pearson partial correlation between two focal variables while
#' controlling for one or more covariates, using
#' \code{\link[ppcor]{pcor.test}} under the hood.
#' Unlike \code{ppcor::pcor.test()}, the wrapper lets you choose columns with
#' tidyselect syntax and unquoted names.
#'
#' @details
#' \itemize{
#'   \item \code{var1} and \code{var2} must each resolve to exactly one column
#'     of \code{data}; an error is thrown otherwise.
#'   \item \code{controls} can be any tidyselect expression; for example
#'     \code{-c(country, var1, var2)} or \code{where(is.numeric)}.
#'     Columns that overlap with \code{var1} or \code{var2} are silently
#'     discarded, and at least one control variable must remain.
#'   \item Rows containing missing values in any selected column are removed
#'     before the test is performed.
#'   \item The function always returns the Pearson partial correlation
#'     coefficient.  For Spearman or Kendall partials, call
#'     \code{ppcor::pcor.test()} directly.
#' }
#'
#' @param var1,var2 Unquoted column names or tidyselect expressions identifying
#'   the two focal variables.  Each must select exactly one column.
#' @param controls Tidyselect expression specifying the set of covariates to
#'   control for.  Must leave at least one column after \code{var1} and
#'   \code{var2} have been removed.
#' @param data A data frame.
#'
#' @return A one-row tibble with the following numeric columns:
#' \describe{
#'   \item{\code{estimate}}{partial correlation coefficient}
#'   \item{\code{statistic}}{t statistic for testing \eqn{\rho = 0}}
#'   \item{\code{p_value}}{two-sided p-value}
#'   \item{\code{n}}{number of complete cases used}
#'   \item{\code{gn}}{number of control variables}
#' }
#'
#' @examples
#' partial_corr(
#'   var1     = mpg,
#'   var2     = hp,
#'   controls = -c(mpg, hp, cyl), # drop focal vars + cyl
#'   data     = mtcars
#' )
#' @export
partial_corr <- function(var1, var2, controls, data) {
  stopifnot(inherits(data, "data.frame"))

  v1q <- rlang::enquo(var1)
  v2q <- rlang::enquo(var2)
  cq <- rlang::enquo(controls)

  # focal selections
  v1_sel <- tidyselect::eval_select(v1q, data = data)
  v2_sel <- tidyselect::eval_select(v2q, data = data)

  if (length(v1_sel) != 1L || length(v2_sel) != 1L) {
    stop("`var1` and `var2` must each select exactly one column.", call. = FALSE)
  }
  if (identical(names(v1_sel), names(v2_sel))) {
    stop("`var1` and `var2` must refer to different columns.", call. = FALSE)
  }

  # control selections
  c_sel <- tidyselect::eval_select(cq, data = data)
  c_sel <- c_sel[setdiff(names(c_sel), c(names(v1_sel), names(v2_sel)))]
  if (length(c_sel) == 0L) {
    stop("`controls` must select at least one control variable.", call. = FALSE)
  }

  v1_name <- names(v1_sel)
  v2_name <- names(v2_sel)
  c_names <- names(c_sel)

  # subset & drop NAs
  df_sub <- data[, c(v1_name, v2_name, c_names), drop = FALSE]
  df_sub <- df_sub[stats::complete.cases(df_sub), , drop = FALSE]

  res <- ppcor::pcor.test(
    x = df_sub[[v1_name]],
    y = df_sub[[v2_name]],
    z = df_sub[, c_names, drop = FALSE],
    method = "pearson"
  )

  tibble::as_tibble(res) |>
    dplyr::select(estimate, statistic,
      p_value = p.value, n = n, gn = gp
    )
}

#' Residual sum of squares (RSS) for an `lm` object
#'
#' Computes the residual sum of squares, \eqn{\sum_{i}\hat\varepsilon_i^{2}},
#' from a fitted linear model.
#' The quantity is sometimes called the error sum of squares.
#'
#' @param model An object of class \code{"lm"}.
#'
#' @return A single numeric value giving the residual sum of squares for the
#'   fitted model.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' get_rss(fit)
#'
#' @export
get_rss <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class \"lm\".", call. = FALSE)
  }

  sum(residuals.lm(model)^2)
}


#' Residual degrees of freedom of a fitted linear model
#'
#' Returns the residual degrees of freedom, \eqn{n - p - 1}, stored in an
#' \code{\link[stats]{lm}} object.
#' This quantity equals the sample size minus the number of fitted
#' coefficients (intercept + slopes) and represents the amount of independent
#' information left for estimating the error variance.
#'
#' @param model An object of class \code{"lm"}.
#'
#' @return A single integer giving the residual degrees of freedom.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' get_residual_df(fit)
#'
#' @export
get_residual_df <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class \"lm\".", call. = FALSE)
  }

  model[["df.residual"]]
}

#' Tidy ANOVA sums-of-squares table
#'
#' @description
#' Returns **Type I**, **II**, or **III** sums-of-squares for an `aov` or
#' `lm` model as a tibble, leveraging **car** and **broom**.
#'
#' @param model An object of class [`aov`] or [`lm`].
#' @param type  Character string specifying the sums-of-squares type:
#'   `"I"`, `"II"`, or `"III"` (case-insensitive).
#'   Defaults to `"II"`, in line with many applied-statistics conventions.
#' @param ...   Additional arguments passed to [car::Anova()] when
#'   `type` is `"II"` or `"III"`.
#'
#' @return A tibble with one row per model term and columns
#'   `term`, `df`, `sumsq`, `meansq` (where available),
#'   a test statistic (`statistic`, e.g. *F*), and `p.value`.
#'
#' @details
#' * **Computation pathway**
#'   \describe{
#'     \item{Type I}{Calculated by applying [broom::tidy()] directly to the
#'       fitted model object (`aov` or `lm`), which reproduces the sequential
#'       sums-of-squares from `stats::anova()`.}
#'     \item{Type II & III}{Calculated with [car::Anova()]—using
#'       `type = 'II'` or `type = 'III'`—and the resulting object is then converted
#'       to a tibble via [broom::tidy()].}
#'   }
#' @examples
#' fit <- aov(lincome ~ degree * sex, data = gss2021)
#' tidy_anova_ssq(fit) # default Type II
#' tidy_anova_ssq(fit, "I") # Type I
#' tidy_anova_ssq(fit, "III") # Type III
#' @export
tidy_anova_ssq <- function(model, type = c("II", "I", "III"), ...) {
  ## validate model class -----------------------------------------------------
  if (!inherits(model, c("aov", "lm"))) {
    stop("'model' must be an object of class 'aov' or 'lm'", call. = FALSE)
  }

  ## validate & normalise `type` ---------------------------------------------
  type <- toupper(match.arg(type))

  ## dispatch by `type` -------------------------------------------------------
  out <- switch(type,
    "I"   = broom::tidy(model, ...), # Type I via stats::anova
    "II"  = broom::tidy(car::Anova(model, type = 2, ...)),
    "III" = broom::tidy(car::Anova(model, type = 3, ...))
  )

  tibble::as_tibble(out)
}


#' Format an individual term’s F-test from a tidy ANOVA tibble
#'
#' Produces an APA-style string of the form
#' `"F(df1, df2) = f-value, p = .xxx"` for any term in a tibble
#' created by [tidy_anova_ssq()].
#' The numerator degrees of freedom and F statistic come from the
#' requested term’s row; the denominator degrees of freedom come from the
#' `"Residuals"` row.
#'
#' @param anova_tbl A tibble returned by [tidy_anova_ssq()].
#' Must contain columns `term`, `df`, `statistic`, and `p.value`.
#' @param term Character scalar. A value that appears in
#'   `anova_tbl$term`; identifies which F-test to report.
#' @param digits Integer; number of decimal places to show for the
#'   F statistic.  Defaults to 2.
#'
#' @return A length-one character vector such as
#'   `"F(4, 72) = 3.52, p = .011"`.
#'
#' @details
#' \describe{
#'   \item{Numerator df}{Taken from the `df` column of the specified term.}
#'   \item{Denominator df}{Taken from the `df` value on the `"Residuals"`
#'     row. The function errors if that row is missing.}
#'   \item{F statistic & p-value}{From the `statistic` and `p.value`
#'     columns of the specified term.}
#' }
#'
#' @export
#'
#' @examples
#' mod_tbl <- tidy_anova_ssq(aov(mpg ~ factor(cyl) * am, data = mtcars))
#' sprintf_term_fstat(mod_tbl, term_name = "factor(cyl)")
sprintf_term_fstat <- function(anova_tbl, term_name, digits = 2) {
  ## --- sanity checks -------------------------------------------------------
  req_cols <- c("term", "df", "statistic", "p.value")
  if (!all(req_cols %in% names(anova_tbl))) {
    stop(
      "`anova_tbl` must contain columns: ",
      paste(req_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"Residuals" %in% anova_tbl$term) {
    stop("`anova_tbl` must include a 'Residuals' row.", call. = FALSE)
  }

  term_row <- dplyr::filter(anova_tbl, term == term_name)

  if (nrow(term_row) == 0L) {
    stop("Term '", term_name, "' not found in `anova_tbl$term`.", call. = FALSE)
  }

  ## --- pull needed pieces --------------------------------------------------
  df1 <- term_row$df
  df2 <- anova_tbl$df[anova_tbl$term == "Residuals"]
  fstat <- round(term_row$statistic, digits)
  pval <- format_pval(term_row$p.value) # your existing helper

  ## --- glue together -------------------------------------------------------
  sprintf("F(%d, %d) = %.*f, %s", df1, df2, digits, fstat, pval)
}
