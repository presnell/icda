\name{alligators2}
\alias{alligators2}
\docType{data}
\title{ Alligator Food Choice II }
\description{
  Primary food choice, length (meters), gender, and lake where captured
  for 219 Florda alligators.
}
\usage{data(alligators2)}
\format{
  A data frame with 80 observations on the following 5 variables.
  \describe{
    \item{\code{Lake}}{a factor with levels \code{Hancock} \code{Oklawaha} \code{Trafford} \code{George}}
    \item{\code{Gender}}{a factor with levels \code{Male} \code{Female}}
    \item{\code{Size}}{a factor with levels \code{<=2.3} \code{>2.3}}
    \item{\code{Food}}{a factor with levels \code{Fish} \code{Invertebrate} \code{Reptile} \code{Bird} \code{Other}}
    \item{\code{Count}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2002) _Categorical Data Analysis (Second Edition)_ Table
  7.1.
}
\references{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 6.14.
}
\examples{
data(alligators2)
## Table 7.1 from "CDA"
ftable(xtabs(Count ~ Lake + Gender + Size + Food, alligators2),
       row.vars=c("Lake","Gender","Size"))
## Table 6.14 from "ICDA"
ftable(xtabs(Count ~ Lake + Size + Food, alligators2),
       row.vars=c("Lake","Size"))
## Problem 6.3 from "ICDA"
library(reshape2)
gators2w <- dcast(alligators2, Lake + Size ~ Food, sum, value.var="Count")
gators2w
library(VGAM)
gators2.fit1 <-
   vglm(cbind(Fish,Invertebrate,Reptile,Bird,Other) ~ Size + Lake,
        family=multinomial, data=gators2w)
summary(gators2.fit1)
}
\keyword{datasets}
