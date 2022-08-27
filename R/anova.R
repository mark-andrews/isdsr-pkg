#' The Greenhouse & Geisser sphericity correction factor
#' 
#' Calculates the Greenhouse & Geisser epsilon-hat factor for correction of sphericity. 
#' This appeared in Geisser, S., & Greenhouse, S. W. (1958), and Greenhouse & Geisser (1959).
#'
#' @param Sigma A sample covariance matrix.
#'
#' @return The epsilon-hat sphericity correction factor.
#' @export
#'
#' @examples
gg_epsilon <- function(Sigma){
  
  K <- nrow(Sigma)
  v <- diag(Sigma) # variances
  Sbar <- mean(Sigma)
  
  numerator <- K ^ 2 * (mean(v) - Sbar) ^ 2 
  denominator <- (
    (K-1) * (
      sum(Sigma^2) - 2 * K * sum(rowMeans(Sigma)^2) + K^2 * mean(Sigma)^2 
    )
  )
  
  numerator / denominator
  
}

#' The Huynh-Feldt sphericity correction factor
#' 
#' Calculates the Huynh-Feldt epsilon-tilde factor for correcting sphericity.
#' This appeared in Huynh & Feldt (1976).
#'
#' @param Sigma 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
hf_epsilon <- function(Sigma, n){
  
  k <- nrow(Sigma)
  eps_hat <- gg_epsilon(Sigma)
  
  ( n * (k - 1) * eps_hat - 2 ) / 
    ( (k - 1) * (n - 1 - (k - 1) * eps_hat) )
  
}


#' Simple main effects Anova
#'
#' For each level of one independent variable, perform an Anova with another
#' independent variable.
#' 
#' @param formula The `aov_car` formula
#' @param by The independent variable for each of whose levels a separate Anova
#'   is performed.
#' @param data The data set.
#' @param table_only If TRUE (default), only a data-frame of F-tests is
#'   returned. If FALSE, a list with each `aov_car` model is returned.
#'
#' @return A list of `afex::afex_aov` objects
#' @export
#'
#' @examples
#' simple_main_effects(log_rt ~ Error(subject/condition), by = keyboard, data = behmercrump)
simple_main_effects <- function(formula, by, data, table_only = TRUE){
  
  aov_table_summary <- function(model){
    tibble::as_tibble(model$anova_table, rownames = 'IV')
  }
  
  formula_check <- c(
    formula.tools::is.two.sided(formula),
    formula.tools::op(formula) == '~'
  )
  
  if (!all(formula_check)) {
    stop('The `formula` must be suitable for `aov_car`.')
  }
  
  results <- 
    dplyr::group_by(
      .data = data, {{ by }}) %>%
    tidyr::nest() %>%
    dplyr::mutate(model = purrr::map(
      data, 
      ~afex::aov_car(formula = formula, data = .))
    )
  
  models <- results$model
  names(models) <- results[[ rlang::as_name(rlang::enquo(by)) ]]
  
  if (table_only){
    purrr::map_dfr(models, aov_table_summary, .id = 'by')
  } else {
    models
  }
}