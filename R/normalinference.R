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

#' Prediction interval of a t-distribution
#'
#' @param nu (numeric) The degrees of freedom parameter of the t-distribution.
#'   This is usually, but not necessarily, an integer.
#' @param probability (numeric) The probability (area under curve) within the
#'   interval
#'
#' @return A numeric vector giving the lower and upper bounds of the interval
#'   containing the specified probability.
#' @export
#'
#' @examples
#' t_interval()
#' t_interval(nu = 10, probability = 0.99)
t_interval <- function(nu = 1, probability = 0.95) {
  
  p <- probability + (1 - probability)/2
  
  ub <- qt(p, df = nu)
  c(-ub, ub)
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
#' @param sigma (numeric) If not numeric, the assumed value of the standard deviation
#' @param na.rm (logical) When calculating the sufficient statistics, do we remove NA's
#'
#' @return (numeric) A p-value for the hypothesis
#' @export
#'
#' @examples
#' y <- rnorm(10, mean = 1)
#' normal_inference_pvalue(y, mu = 1)
normal_inference_pvalue <- function(y, mu, sigma = NULL, na.rm = TRUE, verbose = FALSE){
  
  ybar <- mean(y, na.rm = na.rm)
  if (is.null(sigma)){
    s <- sd(y, na.rm = na.rm)
  } else {
    s <- sigma
  }
  
  n <- sum(!is.na(y))
  standard_error <- s/sqrt(n)
  statistic <- (ybar - mu)/standard_error
  
  if (!is.null(sigma)) {
    pvalue <- pnorm(q = abs(statistic), mean = 0, sd = 1, lower.tail = F) * 2
    
    if (!verbose){
      pvalue
    } else {
      tibble::tibble(z = statistic, p = pvalue)
    }
    
  } else {
    pvalue <- pt(q = abs(statistic), df = n - 1, lower.tail = F) * 2
    
    if (!verbose){
      pvalue
    } else {
      tibble::tibble(t = statistic, nu = n - 1, p = pvalue)
    }
    
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

#' Area under t-distribution between two bounds
#'
#' @param lower_bound (numeric) The lower boundary of the interval
#' @param upper_bound (numeric) The upper boundary of the interval
#' @param nu (numeric) The degrees of freedom parameter of the t-distribution.
#'   This is usually, but not necessarily, an integer.
#'
#' @return (numeric) The probability of being between the lower bound and upper
#'   bound in a t distributed random variable whose mean and standard
#'   deviation are those specified.
#' @export
#'
#' @examples
#' t_auc(lower_bound = 0, nu = 1)
#' t_auc(upper_bound = 1, nu = 10)
#' t_auc(lower_bound = -1, upper_bound = 2, nu = 25)
t_auc <- function(lower_bound=-Inf, upper_bound=Inf, nu = 1){
  pt(upper_bound, df = nu) - pt(lower_bound, df = nu)
}


#' Confidence interval for the mean of normal distribution
#'
#' Given a set of values assumed to sampled independently from a normal
#' distribution, calculate the confidence interval of the distribution's mean.
#'
#' The standard deviation of the normal distribution can assumed to be known to
#' have the value of the sample standard deviation. If so, the confidence
#' interval is calculated assuming a normal sampling distribution.
#'
#' If the standard deviation is not assumed to be known, the confidence interval
#' is calculated using the t-distribution.
#' 
#' @param y (numeric) A set of values assumed to be drawn independently from a normal distribution.
#' @param level (numeric) The level of the confidence interval.
#' @param sigma (numeric) If not NULL, the assumed value of the standard deviation
#' @param na.rm (logical) When calculating the sufficient statistics, do we remove NA's
#' 
#' @return (numeric) The lower and upper bounds of the confidence interval
#' @export
#' 
#' @examples
#' y <- rnorm(10, mean = 5)
#' normal_inference_confint(y)
normal_inference_confint <- function(y, level = 0.95, sigma = NULL, na.rm = T){
  
  ybar <- mean(y, na.rm = na.rm)
  if (is.null(sigma)){
    s <- sd(y, na.rm = na.rm)
  } else {
    s <- sigma
  }
  
  n <- sum(!is.na(y))
  standard_error <- s/sqrt(n)
  
  if (!is.null(sigma)) {
    scaling_factor <- qnorm(level + (1-level)/2)
    
  } else {
    scaling_factor <- qt(level + (1-level)/2, df = n-1)
  }
  
  ybar + c(-1, 1) * scaling_factor * standard_error
}

#' Plot a normal QQ-plot
#' 
#' Given a set of values, the quantiles (percentiles) of each value is
#' calculated. The values in a standard normal corresponding to each quantile is
#' calculated. The z-scores of the values of the variable are then plotted
#' against their corresponding z-scores in the standard normal.
#'
#' @param var The variable to be compared to the 
#' @param data The data frame contain the variable
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' qqnormplot(score, mathplacement)
qqnormplot <- function(var, data){
  
  data %>% 
    mutate(y = {{var}},
           z = (y - mean(y))/sd(y)) %>%
    mutate(p = ecdf(y)(y),
           x = qnorm(p)) %>% 
    ggplot(aes(x = x, y = z)) +
    geom_point(size = 0.5) +
    ylab('Sample quantiles') +
    xlab('Theoretical quantiles') +
    geom_abline(intercept = 0, slope = 1, col='red')
}
