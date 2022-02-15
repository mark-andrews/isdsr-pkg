#' Sample red and black marbles from an urn of marbles #'
#' @param red The number of red marbles in the urn.
#' @param black The number of black marbles in the urn.
#' @param sample_size The number of marbles to draw from the urn.
#' @param replace Is the sampling with or without replacement
#' @param repetitions The number of repetitions of this sampling to be done.
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
                        replace = TRUE,
                        repetitions = 1){
  
  N <- red + black # total number of marbles in urn
  
  if (replace){
    
    # sampling with replacement: sample from binomial dist
    red <- rbinom(n = repetitions, 
                  size = sample_size, 
                  prob = red/N)
    
  } else {
    
    # sampling without replace: sample from hypergeometric dist
    red <- rhyper(nn = repetitions,
                  m = red,
                  n = black,
                  k = sample_size)
  }
  
  # return a tibble with two integer columns
  tibble::tibble(
    red = red,
    black = as.integer(sample_size) - red
  )
  
}