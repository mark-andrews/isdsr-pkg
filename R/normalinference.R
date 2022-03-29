#' Prediction interval of a normal distribution
#' 
#' This function calculates the (central) interval of a normal distribution that
#' contains a specified probability. This interval will always extend from a
#' certain distance below the mean to the same distance above the mean.
#'
#' @param mean (numeric) The mean of the normal distribution
#' @param sd (numeric) The standard deviation of the normal distribution
#' @param probability (numeric) The probability (area under curve) within the interval
#'
#' @return A numeric vector giving the lower and upper bounds of the interval containing the specified probability.
#' @export
#'
#' @examples
#' normal_interval()
#' normal_interval(mean = 100, sd = 15, probability = 0.99)
normal_interval <- function(mean = 0, sd = 1, probability = 0.95) {
  
  p <- probability + (1 - probability)/2
  
  ub <- qnorm(p)
  mean + c(-ub, ub) * sd
}