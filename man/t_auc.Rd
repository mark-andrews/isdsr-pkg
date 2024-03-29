% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalinference.R
\name{t_auc}
\alias{t_auc}
\title{Area under t-distribution between two bounds}
\usage{
t_auc(lower_bound = -Inf, upper_bound = Inf, nu = 1)
}
\arguments{
\item{lower_bound}{(numeric) The lower boundary of the interval}

\item{upper_bound}{(numeric) The upper boundary of the interval}

\item{nu}{(numeric) The degrees of freedom parameter of the t-distribution.
This is usually, but not necessarily, an integer.}
}
\value{
(numeric) The probability of being between the lower bound and upper
bound in a t distributed random variable whose mean and standard
deviation are those specified.
}
\description{
Area under t-distribution between two bounds
}
\examples{
t_auc(lower_bound = 0, nu = 1)
t_auc(upper_bound = 1, nu = 10)
t_auc(lower_bound = -1, upper_bound = 2, nu = 25)
}
