#' A two dimensional scatterplot
#'
#' This function is a wrapper around the typical `ggplot` command to create two
#' dimensional scatterplots, i.e. using `geom_point`. It provides the option of
#' colouring point by a third variable, one that is usually, though not
#' necessarily categorical. Also, it provides the option of placing the line of
#' best fit on the scatterplot. If points are coloured by a categorical
#' variable, the a different line of best for each value of the categorical
#' variable is provided.
#'

#' @param x A numeric variable in `data`. Its values are plotted on the x axis.
#' @param y A numeric variable in `data`. Its values are plotted on the y axis.
#' @param data A data frame with the `x` and `y` variables.
#' @param by An optional variable, usually categorical (factor or
#'   character), by which the points in the scatterplot are byed and
#'   coloured.
#' @param best_fit_line A logical variable indicating if the line of best fit
#'   should shown or not.
#' @param xlab The label of the x-axis (defaults to the `x` variable name).
#' @param ylab The label of the y-axis (defaults to the `y` variable name).
#' @return A `ggplot2::ggplot` object, which may be modified with further `ggplot2`
#'   commands.
#' @examples
#' scatterplot(x = attractive, y = trustworthy, data = faithfulfaces)
#' scatterplot(
#'   x = attractive, y = trustworthy, data = faithfulfaces,
#'   xlab = "attractiveness", ylab = "trustworthiness"
#' )
#' scatterplot(
#'   x = attractive, y = trustworthy, data = faithfulfaces,
#'   by = face_sex
#' )
#' scatterplot(
#'   x = trustworthy, y = faithful, data = faithfulfaces,
#'   by = face_sex, best_fit_line = TRUE
#' )
#' @import ggplot2
#' @export
scatterplot <- function(x, y, data, by = NULL, best_fit_line = FALSE, xlab = NULL, ylab = NULL) {
  if (is.null(enexpr(by))) {
    the_aes <- aes(x = {{ x }}, y = {{ y }})
  } else {
    the_aes <- aes(x = {{ x }}, y = {{ y }}, colour = {{ by }})
  }
  p1 <- ggplot(data = data, mapping = the_aes) +
    geom_point()

  if (best_fit_line) {
    p1 <- p1 + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, formula = "y ~ x")
  }

  p1 <- p1 + theme_classic() + scale_colour_brewer(palette = "Set1")

  if (!is.null(xlab)) {
    p1 <- p1 + labs(x = xlab)
  }

  if (!is.null(ylab)) {
    p1 <- p1 + labs(y = ylab)
  }

  p1 + scale_x_continuous(labels = scales::comma)
}

#' A Tukey box-and-whisker plot
#'
#' This function is a wrapper around a typical `ggplot` based box-and-whisker
#' plot, i.e. using `geom_boxplot`, which implements the Tukey variant of the
#' box-and-whisker plot. The `y` variable is the outcome variable whose
#' distribution is represented by the box-and-whisker plot. If the `x` variable
#' is missing, then a single box-and-whisker plot using all values of `y` is
#' shown. If an `x` variable is used, this is used an the independent variable
#' and one box-and-whisker plot is provided for each set of `y` values that
#' correspond to each unique value of `x`. For this reason, `x` is usually a
#' categorical variable. If `x` is a continuous numeric variable, it ideally
#' should have relatively few unique values, so that each value of `x`
#' corresponds to a sufficiently large set of `y` values.
#'


#' @param y The outcome variable
#' @param x The optional independent/predictor/grouping variable
#' @param data The data frame with the `y` and (optionally) `x` values.
#' @param by An optional variable, usually categorical (factor or character), by
#'   which the points in the box-and-whisker plots are grouped and coloured.
#' @param jitter A logical variable, defaulting to `FALSE`, that indicates if
#'   all points in each box-and-whisker plot should be shown as jittered points.
#' @param box_width The width of box in each box-and-whisker plot. The default
#'   used, `box_width = 1/3`, means that boxes will be relatively narrow.
#' @param jitter_width The width of the jitter relative to box width. For
#'   example, set `jitter_width = 1` if you want the jitter to be as wide the
#'   box.
#' @param xlab The label of the x-axis (defaults to the `x` variable name).
#' @param ylab The label of the y-axis (defaults to the `y` variable name).
#' @return A `ggplot2::ggplot` object, which may be modified with further `ggplot2`
#'   commands.
#' @examples
#' # A single box-and-whisker plot
#' tukeyboxplot(y = time, data = vizverb)
#' # One box-and-whisker plot for each value of a categorical variable
#' tukeyboxplot(y = time, x = task, data = vizverb)
#' # Box-and-whisker plots with jitters
#' tukeyboxplot(y = time, x = task, data = vizverb, jitter = TRUE)
#' # `tukeyboxplot` can be used with a continuous numeric variable too
#' tukeyboxplot(y = len, x = dose, data = ToothGrowth)
#' tukeyboxplot(
#'   y = len, x = dose, data = ToothGrowth,
#'   by = supp, jitter = TRUE, box_width = 0.5, jitter_width = 1
#' )
#' @import ggplot2
#' @export
tukeyboxplot <- function(y, x, data,
                         by = NULL,
                         jitter = FALSE,
                         box_width = 1 / 3,
                         jitter_width = 1 / 5, xlab = NULL, ylab = NULL) {
  # If `x` is missing, and so we have one boxplot, use an empty `x` variable
  # with x = ''.
  if (missing(x)) {
    the_aes <- aes(x = "", y = {{ y }})
  } else {
    the_aes <- aes(x = {{ x }}, y = {{ y }})
  }

  # If we have a `by`, set that as the "colour" aesthetic
  if (!is.null(enexpr(by))) {
    the_aes$colour <- enexpr(by)
  }

  # If we have a continuous `x` variable, we need to use aes(group = ...)
  if (!missing(x)) {
    # If we have a `by` variable, we need to group by an interaction
    if (!is.null(enexpr(by))) {
      the_aes$group <- quo(interaction(!!enexpr(x), !!enexpr(by)))
    } else {
      the_aes$group <- enexpr(x)
    }
  }

  # The basic plot
  p1 <- ggplot(data, mapping = the_aes)

  # With jitter,
  # the jitter
  # Set jitter to a fraction of box_width.
  if (jitter) {
    # outliers should be removed as they will be shown in the jitter
    p1 <- p1 + geom_boxplot(width = box_width, outlier.shape = NA)

    if (!is.null(enexpr(by))) {
      p1 <- p1 + geom_jitter(position = position_jitterdodge(dodge.width = box_width, jitter.width = box_width * jitter_width / 2), size = 0.85, alpha = 0.75)
    } else {
      p1 <- p1 + geom_jitter(width = box_width * jitter_width / 2, size = 0.85, alpha = 0.75)
    }
  } else {
    p1 <- p1 + geom_boxplot(width = box_width, outlier.size = 0.75)
  }

  # If `x` is missing, we don't want any ticks or labels on 'x' axis.
  if (missing(x)) p1 <- p1 + xlab(NULL) + theme(axis.ticks = element_blank())

  p1 <- p1 + theme_classic() + scale_colour_brewer(palette = "Set1")

  if (!is.null(xlab)) {
    p1 <- p1 + labs(x = xlab)
  }

  if (!is.null(ylab)) {
    p1 <- p1 + labs(y = ylab)
  }

  p1 + scale_y_continuous(labels = scales::comma)
}

#' A histogram
#'
#' This is a wrapper to the typical `ggplot` based histogram, i.e., using
#' `geom_histogram`. A continuous variable, `x`, is required as an input.
#' Optionally, a `by` categorical variable can be provided.
#'
#' @param x The numeric variable that is to be histogrammed.
#' @param data A data frame with at least one numeric variable (the `x`
#'   variable).
#' @param by A categorical variable by which to group the `x` values. If
#'   provided there will be one histogram for each set of `x` values grouped by
#'   the values of the `by` variable.
#' @param position If the `by` variable is provided, there are three ways these
#'   multiple histograms can be positioned: stacked (`position = 'stack'`), side
#'   by side (`position = 'dodge'`), superimposed (`position = identity'`).
#' @param facet A character string or character vector. If provided, we
#'   `facet_wrap` (by default) the histogram by the variables. This is
#'   equivalent to the `facet_wrap(variables)` in `ggplot2`.
#' @param facet_type By default, this takes the value of `wrap`, and `facet`
#'   leads to a facet wrap. If `facet_type` is `grid`, then `facet` gives us a
#'   `facet_grid`.
#' @param bins The number of bins to use in the histogram.
#' @param alpha The transparency to for the filled histogram bars. This is
#'   probably only required when using `position = 'identity'`.
#' @param xlab The label of the x-axis (defaults to the `x` variable name).
#' @param ylab The label of the y-axis (defaults to the `y` variable name).
#' @return A `ggplot2::ggplot` object, which may be modified with further `ggplot2`
#'   commands.
#' @examples
#' histogram(x = age, data = schizophrenia, by = gender, bins = 20)
#' histogram(x = age, data = schizophrenia, by = gender, position = "identity", bins = 20, alpha = 0.7)
#' histogram(x = age, data = schizophrenia, by = gender, position = "dodge", bins = 20)
#' histogram(x = weight, bins = 20, data = ansur, facet = height_tercile)
#' histogram(
#'   x = weight, bins = 20, data = ansur,
#'   facet = c(height_tercile, age_tercile), facet_type = "grid"
#' )
#' @import ggplot2 dplyr
#' @export histogram
histogram <- function(x, data, by = NULL, position = "stack", facet = NULL, facet_type = "wrap", bins = 10, alpha = 1.0, xlab = NULL, ylab = NULL) {
  if (is.null(enexpr(by))) {
    the_aes <- aes(x = {{ x }})
  } else {
    the_aes <- aes(x = {{ x }}, fill = {{ by }})
  }

  p1 <- ggplot(data, mapping = the_aes) +
    geom_histogram(
      bins = bins,
      colour = "white",
      position = position,
      alpha = alpha
    )

  if (!is.null(enexpr(facet))) {
    # this monstrosity to deal with unquoted, possibly vector, arguments
    # to facet
    facet_expr <- enexpr(facet)

    if (length(facet_expr) == 1) {
      facet_vars <- as.list(facet_expr)
    } else {
      facet_vars <- as.list(facet_expr)[-1]
    }

    quoted_facet_vars <- sapply(facet_vars, rlang::quo_name)

    if (facet_type == "wrap") {
      p1 <- p1 + facet_wrap(quoted_facet_vars, labeller = label_both)
    } else if (facet_type == "grid") {
      p1 <- p1 + facet_grid(quoted_facet_vars, labeller = label_both)
    } else {
      stop(sprintf('facet_type should be "wrap" or "grid" not %s', facet_type))
    }
  }

  if (!is.null(xlab)) {
    p1 <- p1 + labs(x = xlab)
  }

  if (!is.null(ylab)) {
    p1 <- p1 + labs(y = ylab)
  }

  # minimal looks better than classic in a faceted plot
  if (is.null(enexpr(facet))) {
    p1 <- p1 + theme_classic() + scale_fill_brewer(palette = "Set1")
  } else {
    p1 <- p1 + theme_minimal() + scale_fill_brewer(palette = "Set1")
  }

  p1 + scale_x_continuous(labels = scales::comma)
}

#' Interaction plot using expected marginal means
#'
#' @param model A model such as afex::aov_car of a two-way interaction.
#' @param iv1 One of the two independent variables (variable name must be quoted)
#' @param iv2 The other of the two independent variables (variable name must be quoted)
#' @param ylab The y-axis label
#' @param text_size The size of the font in the legend
#'
#' @return A `patchwork` object
#' @export
#'
#' @examples
#' M <- afex::aov_car(nc_erp ~ category + Error(id / change), data = catknowledge)
#' twoway_interaction_plot(M, "category", "change", ylab = "Nc ERP amplitude")
twoway_interaction_plot <- function(model, iv1, iv2, ylab, text_size = 8) {
  IV1 <- rlang::sym(iv1)
  IV2 <- rlang::sym(iv2)

  formula_1 <- rlang::expr(!!IV1 ~ !!IV2)
  formula_2 <- rlang::expr(!!IV2 ~ !!IV1)

  G <-
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = element_text(size = text_size),
      legend.title = element_text(size = text_size)
    )

  p1 <- emmeans::emmip(model, formula_1, ylab = ylab) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    G

  p2 <- emmeans::emmip(model, formula_2, ylab = ylab) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    G

  p1 + p2 + patchwork::plot_layout(ncol = 2) + patchwork::plot_annotation(tag_levels = "A")
}


#' Correlation Heatmap with Optional Significance Stars
#'
#' Creates a correlation heatmap from selected numeric variables in a data frame,
#' optionally annotating correlation coefficients with significance stars.
#'
#' @param data A data frame or tibble.
#' @param vars A tidyselect specification of numeric variables to include (e.g., -country, where(is.numeric), c(x, y, z)).
#' @param sig Logical. If TRUE, annotate correlations with significance stars (***, **, *, ⁰). If FALSE, show plain correlations only.
#'
#' @return A `ggplot2` heatmap of pairwise correlations.
#' @export
#'
#' @examples
#' corr_plot(whr2024, -country)
#' corr_plot(whr2024, c(happiness, hle, gdp), sig = FALSE)
corr_plot <- function(data, vars, sig = TRUE) {
  # Tidyselect evaluation
  vars <- rlang::enquo(vars)
  data_df <- dplyr::select(data, !!vars)

  # Compute correlation and p-value matrices
  cor_mat <- stats::cor(data_df, use = "complete.obs")
  p_mat <- ggcorrplot::cor_pmat(data_df)

  # Get consistent variable names
  var_names <- colnames(cor_mat)

  # Long-format correlation and p matrices
  cor_df <- as.data.frame(cor_mat) |>
    tibble::rownames_to_column("var1") |>
    tidyr::pivot_longer(-var1, names_to = "var2", values_to = "cor")

  p_df <- as.data.frame(p_mat) |>
    tibble::rownames_to_column("var1") |>
    tidyr::pivot_longer(-var1, names_to = "var2", values_to = "p")

  # Merge and annotate
  plot_df <- dplyr::left_join(cor_df, p_df, by = c("var1", "var2")) |>
    dplyr::mutate(
      stars = dplyr::case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        TRUE ~ "⁰"
      ),
      label = if (sig) {
        sprintf("%.2f%s", cor, stars)
      } else {
        sprintf("%.2f", cor)
      },
      var1 = factor(var1, levels = var_names),
      var2 = factor(var2, levels = var_names)
    ) |>
    dplyr::filter(as.integer(var1) < as.integer(var2)) # lower triangle only

  # Create the plot
  ggplot2::ggplot(plot_df, ggplot2::aes(x = var2, y = var1, fill = cor)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = 0, limit = c(-1, 1), space = "Lab",
      name = "Pearson\nCorrelation"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
}
