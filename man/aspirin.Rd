\name{aspirin}
\alias{aspirin}
\docType{data}
\title{ Aspirin and Heart Attacks }
\description{
  Counts of subjects experiencing and not experiencing myocardial
  infarction in the placbo and aspirin arms of the Physician's Health
  Study.
}
\usage{data(aspirin)}
\format{
  A data frame with 4 observations on the following 3 variables.
  \describe{
    \item{\code{Group}}{a factor with levels \code{Placebo} \code{Aspirin}}
    \item{\code{MI}}{a factor with levels \code{Y} \code{N}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 2.3.
}
\examples{
data(aspirin)
}
\keyword{datasets}
