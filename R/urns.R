`%>%` <- magrittr::`%>%`

#' Sample red and black marbles from an urn of marbles 
#' #'
#' @param red The number of red marbles in the urn.
#' @param black The number of black marbles in the urn.
#' @param sample_size The number of marbles to draw from the urn.
#' @param replace Is the sampling with or without replacement?
#' @param repetitions The number of repetitions of this sampling to be done.
#' @param tally If TRUE, instead of returning the data frame of all samples,
#'   return a data frame of the number of times each combination of red and
#'   black marbles occur in the samples.
#' 
#' @return A data frame with two columns: `n_red` and `n_black`, which give the
#'   number of red and black marbles, respectively, in the sample of size
#'   `sample_size`. Each row of the data frame gives the results of the
#'    repetition of this experiment.
#' @export
#'
#' @examples
#' urn_sampler(repetitions = 5)
urn_sampler <- function(red = 50,
                        black = 50,
                        sample_size = 25,
                        red_proportion = NULL,
                        urn_size = NULL,
                        replace = TRUE,
                        repetitions = 1, 
                        tally = FALSE){
  
  N <- red + black # total number of marbles in urn
  
  if (replace){
    
    # sampling with replacement: sample from binomial dist
    red <- rbinom(n = repetitions, 
                  size = sample_size, 
                  prob = red/N)
    
  } else {
    
    # TODO We need to do error checking here
    
    # sampling without replace: sample from hypergeometric dist
    red <- rhyper(nn = repetitions,
                  m = red,
                  n = black,
                  k = sample_size)
  }
  
  # create a tibble with two integer columns
  result_df <- tibble::tibble(
    red = red,
    black = as.integer(sample_size) - red
  )
  
  # if we are not tallying, if repetitions is just 1, then
  # return a vector, else return a data frame
  if (!tally){
    if (repetitions > 1){
      result_df
    } else {
      unlist(result_df)
    }
  } else {
    result_df %>%
      dplyr::group_by(red, black) %>% 
      dplyr::tally(name = 'tally') %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(prob = tally/sum(tally)) %>% 
      dplyr::arrange(red) # sort by `red`, which I think may always happen anyway
  }

}


#' The exact sampling distribution for an urn problem
#'
#' @param red The number of red marbles in the urn.
#' @param black The number of black marbles in the urn.
#' @param sample_size The number of marbles to draw from the urn.
#' @param red_proportion As an alternative to specifying the number of 
#' red and black marbles in the jar, the proportion of red marbles
#' can be specified. If `red_proportion` is specified, then
#' `red` and `black` will be ignored.
#' @param observed_red The number of red marbles in a sample. This
#' is optional. TODO more documentation here please
#' @param replace Is the sampling with or without replacement?
#'
#' @return
#' @export
#'
#' @examples
#' urn_sampling_distribution(sample_size = 10)
urn_sampling_distribution <- function(red = 50,
                                      black = 50,
                                      sample_size = 25,
                                      red_proportion = NULL,
                                      observed_red = NULL,
                                      replace = TRUE){
  
  # sequence of all possible values of
  # the number of red marbles in sample
  red_seq <- seq(0, sample_size) 
  
  result <- tibble(red = red_seq, 
                   black = as.integer(sample_size) - red)
  
  if (is.null(red_proportion)) {
    
    N <- red + black # total number of marbles in the urn

    if (replace){
      prob <- dbinom(red_seq, size = sample_size, prob = red/N)
    } else {
      prob <- dhyper(red_seq, m = red, n = black, k = sample_size)
    }
    
    dplyr::mutate(result, prob = prob)
    
  } else {
    
    # TODO implement for case of without replacement
    if (!replace) stop('Not yet implemented for sampling without replacement') 
    
    prob = dbinom(red_seq, size = sample_size, prob = red_proportion)
    
    sampling_distribution_centre <- sample_size * red_proportion
    
    if (!is.null(observed_red)) {
      observed_result_dist_from_centre <- abs(observed_red - sampling_distribution_centre)
      
      epsilon <- 1e-10
      
      dplyr::mutate(result,
                    prob = prob,
                    delta = red - sampling_distribution_centre,
                    as_extreme = abs(abs(delta) - observed_result_dist_from_centre) < epsilon,
                    more_extreme = abs(abs(delta) > observed_result_dist_from_centre) > epsilon,
                    as_or_more_extreme = as_extreme | more_extreme) %>%
        dplyr::select(red, black, prob, as_or_more_extreme)
      
    } else {
      
      dplyr::mutate(result, prob = prob)
    }
    

    
  }


}

#' Calculate p-value for a hypothesis in binomial problem
#'
#' Assuming that we have observed a certain number of successes from a binomial
#' distribution, obtain the p-value for a hypothesis about the binomial
#' distribution probability parameter, i.e. the probability of a success on each
#' trial.
#' 
#' @param sample_size The sample size.
#' @param hypothesis The hypothesized probability parameter.
#' @param observed The observed number of successes in the sample.
#'
#' @return
#' @export
#' @import purrr
#'
#' @examples
#' # p-value for hypothesis that prob=0.4, given 3 successes in 10 trials
#' binomial_pvalue(10, 3, 0.4)
binomial_pvalue <- function(sample_size, observed, hypothesis) {
  map_dbl(magrittr::set_names(hypothesis, hypothesis), 
          ~binom.test(x = observed, n = sample_size, p = .)$p.value
  )
}

#' @describeIn binomial_pvalue Calculate s-value for a hypothesis in a binomial problem
#' @export
binomial_svalue <- function(sample_size, observed, hypothesis) {
  -log2(binomial_pvalue(sample_size, observed, hypothesis))
}


#' @describeIn binomial_pvalue Calculate confidence interval in a binomial problem
#' @export
binomial_confint <- function(sample_size, observed, level = 0.95) {
  conf_interval <- binom.test(x = observed, 
                              n = sample_size, 
                              conf.level = level)$conf.int
  attributes(conf_interval) <- NULL
  alpha <- 1 - level
  magrittr::set_names(conf_interval, c(alpha/2, 1-alpha/2))
}

likelihood_interval_roots <- function(m, n, z=1/4) {
  
  N <- n
  n <- m
  
  likelihood <- function(x, N, n){
    x^n * (1-x)^(N-n)
  }
  
  f <- function(x, N, n, z) {
    standardized.likelihood <- likelihood(x, N, n) / likelihood(n/N, N, n)
    standardized.likelihood - z
  }
  
  left_root <- uniroot(f, 
                       c(0, n/N), 
                       tol = 0.0001, 
                       N = N, 
                       n=n,
                       z=z)
  
  right_root <- uniroot(f, 
                        c(n/N, 1), 
                        tol = 0.0001, 
                        N = N, 
                        n=n,
                        z=z)
  
  list(x = left_root$root,
       y = likelihood(left_root$root, N, n),
       xend = right_root$root,
       yend = likelihood(right_root$root, N, n))
}



#' Plot the likelihood function of a binomial model
#'
#' @export
#' @param m Number of successes
#' @param n Number of trials
#' @param lim x-axis limits
#' @param interval Show likelihood interval? If NULL, no interval, if numeric,
#'   show likelihood interval 1/2^interval
#' @return A ggplot object
#' @examples
#' binomial_likelihood(250, 135)
#' @import tibble
#' @import ggplot2
binomial_likelihood_plot <- function(m, n, lim = c(0, 1), interval = NULL){

  data_df <- tibble(x = seq(lim[1], lim[2], length.out = 1000),
                    y = x^m * (1-x)^(n-m))
  
  p1 <- data_df %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    labs(x = latex2exp::TeX("$\\theta$"),
         y = latex2exp::TeX("$L(\\theta | n, m)$")) +
    #ggtitle(sprintf("%d successes in %d trials", m, n)) +
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
  
  if (is.null(interval)){
    p1
  } else {
  
    roots <- likelihood_interval_roots(m, n, z = 1/2^interval)
    
    p1 + geom_segment(aes(x = roots$x,
                          y = roots$y,
                          xend = roots$xend,
                          yend = roots$yend),
                      col='red')
  }

}

#' Plot a Beta distribution
#'
#' @param alpha First shape parameter of the Beta distribution.
#' @param beta Second shape parameter of the Beta distribution.
#' @param show_hpd Show the HPD interval as a line segment.
#' @param level Amount of mass in the HPD interval.
#' @return A ggplot object.
#' @import tibble
#' @import ggplot2
#' @examples
#' beta_plot(2, 2)
#' beta_plot(10, 5)
#' @export
beta_plot <- function(alpha, beta, show_hpd = FALSE, level = 0.95, xlim = c(0, 1)){
  
  data_df <- tibble(x = seq(xlim[1], xlim[2], length.out = 1000),
                    y = dbeta(x, shape1 = alpha, beta))
  
  p <- data_df %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    labs(x = latex2exp::TeX("$\\theta$"),
         y = latex2exp::TeX("$P(\\theta)$")) +
    #ggtitle(sprintf("Beta(%2.1f, %2.1f)", alpha, beta)) +
    theme_classic()
  
  if (show_hpd){
    hpd_interval <- get_beta_hpd(alpha, beta, level = level)
    p + geom_segment(aes(x = hpd_interval$lb,
                         xend = hpd_interval$ub,
                         y = 0,#hpd_interval$p_star,
                         yend = 0),#hpd_interval$p_star),
                     colour = 'red',
                     size = 0.5,
                     data = NULL)
  } else {
    p
  }
  
}

#' Plot the posterior distribution of a binomial model with Beta prior
#'
#' @param n Number of trials
#' @param m Number of successes
#' @param alpha First shape parameters of the Beta prior
#' @param beta Second shape parameter of the Beta prior
#' @param show_hpd Show the HPD interval
#' @param level The amount of mass in HPD interval
#' @param xlim The limits on x-axis
#' @return
#' @examples
#' binomial_posterior_plot(250, 139, 5, 5)
#' binomial_posterior_plot(100, 60, 2, 2, level = 0.99)
#' @export
binomial_posterior_plot <- function(n, m, alpha=1, beta=1, show_hpd = TRUE, level = 0.95, xlim = c(0, 1)){
  beta_plot(m + alpha, n - m + beta, show_hpd = show_hpd, level = level, xlim = xlim)
}

#' The high posterior density interval of a Beta distribution.
#' 
#' @param alpha First shape parameter of the Beta distribution.
#' @param beta Second shape parameter of the Beta distribution.
#' @return A list with lower and upper bound of the HPD interval, and minimum density of interval.
#' @examples
#' get_beta_hpd(10, 5)
#' @export
get_beta_hpd <- function(alpha, beta, level = 0.95){
  
  # This will break if either alpha < 1.0 or beta < 1.0
  # or alpha = beta = 1.0
  stopifnot(alpha >= 1.0, beta >= 1.0, !((alpha == 1) & (beta ==1)))
  
  integrand_lb <- qbeta(1e-4, alpha, beta)
  integrand_ub <- qbeta(1 - 1e-5, alpha, beta)
  beta_mode <- (alpha-1)/(alpha+beta-2)
  
  interval_mass <- function(p_star){
    # Return the area under the curve for the
    # set of points whose density >= p_star.
    
    f <- function(val){
      d <- dbeta(val, alpha, beta)
      if (d >= p_star){
        d
      }
      else {
        0
      }
    }
    
    integrate(Vectorize(f), 
              integrand_lb, 
              integrand_ub, 
              subdivisions = 10000L)$value
    
  }
  
  err_fn <- function(p_star, hpd_mass=level){
    (hpd_mass-interval_mass(p_star))^2
  }
  

  
  max_f <- dbeta(beta_mode, alpha, beta)
  
  Q <- optimize(err_fn,
                interval = c(0, max_f)
  )
  
  precision <- 3
  p_star <- round(Q$minimum, precision)
  
  dbeta_pstar <- function(x, alpha, beta, pstar) {
    dbeta(x, alpha, beta) - pstar
  }

  left_root <- uniroot(dbeta_pstar, 
                       c(integrand_lb, beta_mode), 
                       tol = 0.0001, 
                       alpha = alpha,
                       beta = beta,
                       pstar = p_star)
  
  right_root <- uniroot(dbeta_pstar, 
                        c(beta_mode, integrand_ub), 
                        tol = 0.0001, 
                        alpha = alpha,
                        beta = beta,
                        pstar = p_star)
  
  list(lb = left_root$root,
       ub = right_root$root,
       p_star = p_star)
  
}

#' The high posterior density interval in a binomial problem with Beta prior.
#' 
#' @param m Number of successes
#' @param n Number of trials
#' @param alpha First shape parameter of the Beta prior distribution.
#' @param beta Second shape parameter of the Beta prior distribution.
#' @return A vector with lower and upper bound of the HPD interval.
#' @examples
#' binomial_posterior_hpd(172, 1300)
#' @export
binomial_posterior_hpd <- function(sample_size, observed, alpha=1, beta=1, level = 0.95){
  n <- sample_size
  m <- observed
  hpd_interval <- get_beta_hpd(m + alpha, n - m + beta, level = level)
  
  hpd <- c(hpd_interval$lb, hpd_interval$ub)
  names(hpd) <- c((1 - level)/2, level + (1 - level)/2)
  hpd
}

#' Summary statistics of a Beta distribution
#'
#' @param alpha First shape parameter of the Beta distribution
#' @param beta Second shape parameter of the Beta distribution
#' @return A list with summary statistics of the Beta distribution
#' @examples
#' beta_summary(3, 5)
#' @export
beta_summary <- function(alpha, beta){
  mean <- alpha/(alpha+beta)
  var <- (alpha*beta)/( (alpha+beta)^2 * (alpha + beta + 1))
  sd <- sqrt(var)
  mode <- (alpha - 1)/(alpha + beta - 2)
  
  # list(mean = mean,
  #      var = var,
  #      sd = sqrt(var),
  #      mode = mode)
  c(mean = mean, sd = sqrt(var))
}

#' Summary stats of posterior distribution of binomial model with Beta prior
#'
#' @param n Number of trials
#' @param m Number of successes
#' @param alpha First shape parameter of the Beta prior
#' @param beta Second shape parameter of the Beta prior
#' @return A list with summary statistics of the posterior distribution
#' @examples
#' binomial_posterior_summary(3, 5)
#' @export
binomial_posterior_summary <- function(sample_size, observed, alpha=1, beta=1){
  n <- sample_size
  m <- observed
  beta_summary(m + alpha, n - m + beta)
}