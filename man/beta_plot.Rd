% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/urns.R
\name{beta_plot}
\alias{beta_plot}
\title{Plot a Beta distribution}
\usage{
beta_plot(alpha, beta, show_hpd = FALSE, level = 0.95, xlim = c(0, 1))
}
\arguments{
\item{alpha}{First shape parameter of the Beta distribution.}

\item{beta}{Second shape parameter of the Beta distribution.}

\item{show_hpd}{Show the HPD interval as a line segment.}

\item{level}{Amount of mass in the HPD interval.}
}
\value{
A ggplot object.
}
\description{
Plot a Beta distribution
}
\examples{
beta_plot(2, 2)
beta_plot(10, 5)
}
