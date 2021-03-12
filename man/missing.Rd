\name{missingpersons}
\alias{missingpersons}
\docType{data}
\title{ Missing Persons in London }
\description{
  Persons reported missing in London in year ending March 1993
  cross-classified according to age, gender, and whether still missing a
  year later.
}
\usage{data(missingpersons)}
\format{
  A data frame with 6 observations on the following 4 variables.
  \describe{
    \item{\code{gender}}{a factor with levels \code{M} \code{F}}
    \item{\code{age}}{a factor with levels \code{<14} \code{14-18} \code{>18}}
    \item{\code{stillmissing}}{a numeric vector}
    \item{\code{total}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Exercise 5.15.  
}
\references{
  _The Independent_ March 8, 1994.
}
\examples{
data(missingpersons)
missing.fit1 <- glm(stillmissing/total ~ gender + age, weights=total,
                    family=binomial, data=missingpersons)
summary(missing.fit1)
drop1(missing.fit1, test="Chisq")
missing.fit2 <- update(missing.fit1, . ~ gender + I(age == ">18"))
anova(missing.fit2, missing.fit1, test="Chisq")
}
\keyword{datasets}
