\name{teens}
\alias{teens}
\docType{data}
\title{
  Alcohol, Cigarette, and Marijuana Use for High School Seniors
}
\description{
  A table cross-classifying 2276 American high school seniors by race,
  gender, and their usage of alcohol, cigarettes, and marijuana.
}
\usage{data(teens)}
\format{
  The format is:
 table [1:2, 1:2, 1:2, 1:2, 1:2] 405 13 1 1 268 218 17 117 453 28 ...
 - attr(*, "dimnames")=List of 5
  ..$ C: chr [1:2] "Yes" "No"
  ..$ A: chr [1:2] "Yes" "No"
  ..$ M: chr [1:2] "Yes" "No"
  ..$ G: chr [1:2] "Female" "Male"
  ..$ R: chr [1:2] "White" "Other"
}
\details{
  Taken from Table 7.13 (p. 226) of Agresti (2007). The data were
  provided by Professor Harry Khamis of Wright State University.
}
\source{
  Agresti, A. A. (2007)
  \emph{An Introduction to Categorical Data Analysis} (2nd edition).
  Wiley.
}
\examples{
data(teens)

## Table 7.13, p. 226
ftable(R + G + M ~ A + C, data = teens)

## Table 7.3, p. 209 (collapsing across race and gender)
ftable(M ~ A + C, data = teens)

## Some models from Table 7.4, fitted to marginal ACM table.
acm <- margin.table(teens, 1:3)
library(MASS)
AC.M <- loglm(~ A*C + M, data = acm)
AM.CM <- loglm(~ A*M + C*M, data = acm)
AC.AM.CM <- loglm(~ A*C + A*M + C*M, data = acm)
round(ftable(M ~ A + C, data = fitted(AC.AM.CM)), 1)

## Some models from Table 7.14, p. 227

## Model 1.
loglm(~ A + C + M + G*R, data = teens)

## Model 2.
loglm(~ A*C + A*M + C*M + A*G + A*R + C*G + C*R + M*G + M*R + G*R, data = teens)

## Model 6.
loglm(~ A*C + A*M + C*M + A*G + A*R + G*M + G*R, data = teens)

}
\keyword{datasets}
