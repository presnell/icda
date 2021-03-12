\name{afterlife}
\alias{afterlife}
\docType{data}
\title{ Belief in Afterlife }
\description{
  Data from the General Social Survey cross-classifying respondents by
  race, gender, and their level of belief a life after death.
}
\usage{data(afterlife)}
\format{
  A data frame with 12 observations on the following 4 variables.
  \describe{
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Race}}{a factor with levels \code{White} \code{Black}}
    \item{\code{Believe}}{a factor with levels \code{Yes} \code{Undecided} \code{No}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 6.4.
}
\examples{
data(afterlife)
library(reshape2)
afterw <- dcast(afterlife, Race + Gender ~ Believe, value.var="Freq")
afterw
library(VGAM)
afterw.fit1 <-
   vglm(cbind(Yes,Undecided,No) ~ Gender + Race,
        family=multinomial, data=afterw)
}
\keyword{datasets}
