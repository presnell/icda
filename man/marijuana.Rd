\name{marijuana}
\alias{marijuana}
\docType{data}
\title{
  Alcohol, Cigarette, and Marijuana Use for High School Seniors
}
\description{
  A table cross-classifying 2276 American high school seniors by race,
  gender, and their usage of alcohol, cigarettes, and marijuana.
}
\usage{data(marijuana)}
\format{
  The format is:
 table [1:2, 1:2, 1:2, 1:2, 1:2] 405 13 1 1 268 218 17 117 453 28 ...
 - attr(*, "dimnames")=List of 5
  ..$ Cigarettes: chr [1:2] "Yes" "No"
  ..$ Alcohol   : chr [1:2] "Yes" "No"
  ..$ Marijuana : chr [1:2] "Yes" "No"
  ..$ Gender    : chr [1:2] "Female" "Male"
  ..$ Race      : chr [1:2] "White" "Other"
}
\details{
  Taken from Table 7.13 (p. 226) of Agresti (2007), these data were
  provided by Professor Harry Khamis of Wright State University.
}
\source{
  Agresti, A. A. (2007)
  \emph{An Introduction to Categorical Data Analysis} (2nd edition).
  Wiley.
}
\examples{
data(marijuana)

## Table 7.13, p. 226
ftable(Race + Gender + Marijuana ~ Alcohol + Cigarettes, data=marijuana)

## Table 7.3, p. 209 (collapsing across race and gender)
ftable(Marijuana ~ Alcohol + Cigarettes, data=marijuana)
}
\keyword{datasets}
