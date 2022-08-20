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