\name{traincollisions}
\alias{traincollisions}
\docType{data}
\title{Collisions involving trains in Great Britain}
\description{
  Millions of kilometers travelled annually and counts of two types of
  train-related accidents in the UK between 1975 and 2003: accidents
  involving trains alone (collisions, derailments, and overruns); and
  collisions between trains and road vehicles.
}
\usage{data(traincollisions)}
\format{
  A data frame with 29 observations on the following 4 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{KM}}{a numeric vector}
    \item{\code{Train}}{a numeric vector}
    \item{\code{TrRd}}{a numeric vector}
  }
}
\source{
  British Department of Transport, reported in Table 3.4 of Agresti
  (2007).
}
\references{
  Alan Agresti, \emph{An Introduction to Categorical Data Analysis, 2nd
    ed.}, Wiley, (2007), Table 3.4.
}
\examples{
data(traincollisions)
glm(TrRd ~ I(Year-1975), offset = log(KM), family=poisson,
    data=traincollisions)
}
\keyword{datasets}
