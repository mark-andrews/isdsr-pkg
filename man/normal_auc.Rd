% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalinference.R
\name{normal_auc}
\alias{normal_auc}
\title{Area under normal distribution between two bounds}
\usage{
normal_auc(lower_bound = -Inf, upper_bound = Inf, mean = 0, sd = 1)
}
\arguments{
\item{lower_bound}{(numeric) The lower boundary of the interval}

\item{upper_bound}{(numeric) The upper boundary of the interval}

\item{mean}{(numeric) The mean of the normal distribution}

\item{sd}{(numeric) The standard deviaion of the normal distribution}
}
\value{
(numeric) The probability of being between the lower bound and upper
bound in a normally distributed random variable whose mean and standard
deviation are those specified.
}
\description{
Area under normal distribution between two bounds
}
\examples{
normal_auc(lower_bound = 0)
normal_auc(upper_bound = 1)
normal_auc(lower_bound = -1, upper_bound = 2)
}
