\name{malformation}
\alias{malformation}
\docType{data}
\title{Infant Malformation and Mother's Alcohol Consumption}
\description{
  Frequency of congenital sex organ malformation with Mother's alcohol
  consumption, measured as average number of drinks per day.
}
\usage{data(malformation)}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{Alcohol}}{a numeric vector with 0 representing no
      drinks, 0.5 representing less than 1 drink per day on average, 1.5
    representing 1--2 drinks, 4 representing 3--5 drinks, and 7
    representing 6 or more drinks per day on average.}
    \item{\code{Malformation}}{a factor with levels \code{Absent}
      \code{Present}}
    \item{\code{Freq}}{a numeric vector of counts}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 2.7.
}
\references{
  B. I. Graubard and E. L. Korn (1987), _Biometrics_, 43: 471--476.
}
\examples{
data(malformation)
xtabs(Freq ~ Alcohol + Malformation, data=malformation)
malform.wd <- 
  reshape(malformation,
          idvar="Alcohol", timevar="Malformation",
          direction="wide")
glm(cbind(Freq.Present,Freq.Absent) ~ Alcohol, 
    family=binomial(link=make.link("identity")),
    data=malform.wd)
}
\keyword{datasets}
