\name{jobsatisfaction}
\alias{jobsatisfaction}
\docType{data}
\title{ Job Satisfaction and Income }
\description{
  Data from the 1991 General Social Survey categorizing 104 subjects by
  gender, income, and level of job satisfaction.
}
\usage{data(jobsatisfaction)}
\format{
  A data frame with 4 observations on the following 3 variables:
  \describe{
    \item{\code{Gender}}{ a factor with levels \code{M} \code{F} }
    \item{\code{Income}}{ a numeric vector }
    \item{\code{JobSat}}{ a numeric vector }
    \item{\code{Freq}}{ a numeric vector }
  }
}
\details{
  Income is coded as 3, 10, 20, and 35, representing incomes less than
  5000 dollar, from 5000 to 15,000, from 15,000 to 25,000, and greater
  than 25,000, respectively.  Job satisfaction is coded as 1, 3, 4, and
  5, representing the categories very dissatisfied, a little
  satisfied, moderately satisfied, and very satisfied.  Job satisfaction
  is clearly an ordinal factor, and income should probably be treated as
  an ordinal factor as well.  Freq gives the frequency for each
  combination of levels of the three factors.
}
\source{
  Alan Agresti, \emph{An Introduction to Categorical Data Analysis,
    2nd edn}, Wiley, (2007), Table 6.12, p. 193.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(jobsatisfaction)
}
\keyword{datasets}
