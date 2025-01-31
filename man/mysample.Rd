% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mysample.R
\name{mysample}
\alias{mysample}
\title{mysample}
\usage{
mysample(n, iter = 10, time = 0.5)
}
\arguments{
\item{n}{- the number of events per trial}

\item{iter}{the number of independent trials. Initially set to 10}

\item{time-}{the time until the function timesout. Initially set to .5

This funcion runs a specified number of trials that records the frequency that the numbers 1:10
are randomly selected. For each trial a bar graph of the frequencies for each number is created}
}
\value{
- makes bar graphs for each trial/iteration
}
\description{
mysample
}
\examples{
mysample(n=100, iter=5, time=1)
}
