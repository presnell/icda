\name{alligators1}
\alias{alligators1}
\docType{data}
\title{ Alligator Food Choice I }
\description{
  Length (meters) and primary food type (Fish (F), Invertebrate (I), or
  Other (O)) for 59 alligators sampled in Lake George, Florda.
}
\usage{data(alligators1)}
\format{
  A data frame with 59 observations on the following 2 variables.
  \describe{
    \item{\code{Length}}{a numeric vector}
    \item{\code{Food}}{a factor with levels \code{F} \code{I} \code{O}}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 6.1.
}
\examples{
data(alligators1)
library(VGAM)
alligators.fit1 <- vglm(Food ~ Length, family=multinomial, data=alligators1)
summary(alligators.fit1)
}
\keyword{datasets}
