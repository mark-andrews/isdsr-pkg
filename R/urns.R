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
binomial_svalue <- function(sample_size, observed, hypothesis) {
  -log2(binomial_pvalue(sample_size, observed, hypothesis))
}