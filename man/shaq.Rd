\name{shaq}
\alias{shaq}
\docType{data}
\title{
  Shaquille O'Neal Free Throw Shooting
}
\description{
  Number of free throw attempted and made by Shaquille O'Neal in 23
  games of the 2000 NBA playoffs.
}
\usage{data(shaq)}
\format{
  A data frame with 23 observations on the following 3 variables.
  \describe{
    \item{\code{Game}}{game index}
    \item{\code{Made}}{number of free throws made}
    \item{\code{Att}}{number of free throws attempted}
  }
}
\details{
  Taken from Table 10.9 (p. 319) of Agresti (2007).
}
\source{
  Agresti, A. A. (2007)
  \emph{An Introduction to Categorical Data Analysis} (2nd edition).
  Wiley.
}
\examples{
data(shaq)
library(lme4)
shaq.glmm <-
   glmer(cbind(Made, Att - Made) ~ 1 + (1 | Game),
         family = binomial, data = shaq)
summary(shaq.glmm)
}
\keyword{datasets}
