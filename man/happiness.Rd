\name{happiness}
\alias{happiness}
\docType{data}
\title{Happiness and Income}
\description{
  Data from the 2002 General Social Survey categorizing persons by their
  income and perceived happiness.
}
\usage{data(happiness)}
\format{
  A data frame with 9 observations of the following 3 variables:
  \describe{
    \item{\code{Income}}{ a factor with levels \code{AboveAvg},
      \code{Avg}, and \code{BelowAvg} }
    \item{\code{Happiness}}{ a factor with levels \code{NotTooHappy},
      \code{PrettyHappy}, and \code{VeryHappy} }
    \item{\code{Freq}}{ a numeric vector of counts}
  }
}
\source{
  Alan Agresti, \emph{An Introduction to Categorical Data Analysis,
    2nd edn}, Wiley, (2007), Table 2.13, p. 59.
}
\examples{
data(happiness)
}
\keyword{datasets}
