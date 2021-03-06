% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/symbolic_objects.R
\name{sym.interval}
\alias{sym.interval}
\title{Create an symbolic_interval type object}
\usage{
sym.interval(x = numeric(), .min = min, .max = max)
}
\arguments{
\item{x}{numeric vector}

\item{.min}{function that will be used to calculate the minimum interval}

\item{.max}{function that will be used to calculate the maximum interval}
}
\value{
a symbolic interval
}
\description{
Create an symbolic_interval type object
}
\examples{
sym.interval(c(1, 2, 4, 5))
sym.interval(1:10)
}
