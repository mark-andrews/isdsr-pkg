#' Calculate the mode of binned variable
#'
#' @param var The variable 
#' @param n_bins The number of bins with which to bin up the variable
#' @param data The data set
#'
#' @return The mid point of the bin that occurs with the highest frequency.
#' @export
#'
#' @examples
#' bin_mode(log_rt, n_bins = 5, data = behmercrump)
bin_mode <- function(var, n_bins, data){
  x <- dplyr::pull(data, {{var}})
  h <- hist(x, breaks = n_bins, plot = F)
  h$mids[which.max(h$counts)]
}
