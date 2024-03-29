% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_and_min.R
\name{localMinima}
\alias{localMinima}
\title{Finds local minima of a vector}
\usage{
localMinima(x)
}
\arguments{
\item{x}{Any vector}
}
\value{
A list of local minimas in the vector
}
\description{
Takes in a vector and finds the local minimas.  (Credit user Tommy https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima).
}
\examples{
\donttest{minimas <- localMinima(x)}

}
