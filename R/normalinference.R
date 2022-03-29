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


#' Calculate the p-value for a hypothesis test about mean of normal distribution
#' 
#' Given a set of values assumed to sampled independently from a normal distribution,
#' calculate the p-value for any hypothetical value of the distribution's mean.
#' 
#' The standard deviation of the normal distribution can assumed to be known to have the value of the sample standard deviation.
#' If so, the p-value is calculated assuming a normal sampling distribution.
#' 
#' If the standard deviation is not assumed to be known, the p-value is calculated using the t-distribution.
#'
#' @param y (numeric) A set of values assumed to be drawn independently from a normal distribution.
#' @param mu (numeric) The hypothesized mean of the normal distribution.
#' @param sigma_known (logical) Is sigma, the standard deviation of the normal distribution, known?
#' @param na.rm (logical) When calculating the sufficient statistics, do we remove NA's
#'
#' @return (numeric) A p-value for the hypothesis
#' @export
#'
#' @examples
#' y <- rnorm(10, mean = 1)
#' normal_inference_pvalue(y, mu = 1)
normal_inference_pvalue <- function(y, mu, sigma_known = FALSE, na.rm = T){
  
  ybar <- mean(y, na.rm = na.rm)
  s <- sd(y, na.rm = na.rm)
  n <- sum(!is.na(y))
  standard_error <- s/sqrt(n)
  statistic <- (ybar - mu)/standard_error
  
  if (sigma_known) {
    pnorm(q = abs(statistic), mean = 0, sd = 1, lower.tail = F) * 2
    
  } else {
    pt(q = abs(statistic), df = n - 1, lower.tail = F) * 2
  }
  
}


#' Area under normal distribution between two bounds
#'
#' @param lower_bound (numeric) The lower boundary of the interval
#' @param upper_bound (numeric) The upper boundary of the interval
#' @param mean (numeric) The mean of the normal distribution
#' @param sd (numeric) The standard deviaion of the normal distribution
#'
#' @return (numeric) The probability of being between the lower bound and upper
#'   bound in a normally distributed random variable whose mean and standard
#'   deviation are those specified.
#' @export
#'
#' @examples
#' normal_auc(lower_bound = 0)
#' normal_auc(upper_bound = 1)
#' normal_auc(lower_bound = -1, upper_bound = 2)
normal_auc <- function(lower_bound=-Inf, upper_bound=Inf, mean = 0, sd = 1){
  pnorm(upper_bound, mean = mean, sd = sd) - pnorm(lower_bound, mean = mean, sd = sd)
}