\name{wafers}
\alias{wafers}
\docType{data}
\title{Imperfections in silicon wafers}
\description{
  Number of imperfects in each of 20 silicon wafers, manufactured under
  two different treaments and at two different thicknesses.
}
\usage{data(wafers)}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{trt}}{a factor with levels \code{A} \code{B}}
    \item{\code{thickness}}{a factor with levels \code{low} \code{high}}
    \item{\code{defects}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Exercises 3.11 and 3.12.  
}
\examples{
data(wafers)
## maybe str(wafers) ; plot(wafers) ...
}
\keyword{datasets}
