% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_and_min.R
\name{localMaxima}
\alias{localMaxima}
\title{Finds local maxima of a vector}
\usage{
localMaxima(x)
}
\arguments{
\item{x}{A 1D array or vector}
}
\value{
A list of local maximas in the vector
}
\description{
Takes in a vector and finds the local maximas.  (Credit user Tommy https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima).
}
\examples{
\donttest{maximas <- localMaxima(x)}

}
