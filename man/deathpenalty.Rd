\name{deathpenalty}
\alias{deathpenalty}
\docType{data}
\title{Race and the Death Penalty in Florida}
\description{
  Frequency of the death penalty in 674 indictments involving multiple
  murders in Florida between 1976 and 1987.  Cases are classified by
  race of victim and race of defendant.
}
\usage{data(deathpenalty)}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{DeathPenalty}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{Defendant}}{a factor with levels \code{White} \code{Black}}
    \item{\code{Victim}}{a factor with levels \code{White} \code{Black}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 2.10.
}
\references{
  M. L. Radelet and G. L. Pierce (1991), _Florida Law Review_, 43:
  1--34, 1991.
}
\examples{
data(deathpenalty)
deathpenalty <-
  transform(deathpenalty,
            DeathPenalty = relevel(DeathPenalty, "Yes"),
            Defendant = relevel(Defendant, "White"),
            Victim = relevel(Victim, "White"))
dp <- xtabs(Freq ~ Victim + Defendant + DeathPenalty, data=deathpenalty)
ftable(DeathPenalty ~ Victim + Defendant, data=dp)
## Model for Problem 7.5
DV.DP.PV <-
  glm(Freq ~ Defendant*Victim + Defendant*DeathPenalty + DeathPenalty*Victim,
      family=poisson, data=deathpenalty)
}
\keyword{datasets}
