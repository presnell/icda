\name{horseshoecrabs}
\alias{horseshoecrabs}
\docType{data}
\title{ Horseshoe Crabs }
\description{
  Color, spine condition, carapace width (cm), weight (kg), and number
  of male satellites for 173 female horseshoe crabs.
}
\usage{data(horseshoecrabs)}
\format{
  A data frame with 173 observations on the following 5 variables.
  \describe{
    \item{\code{Color}}{a numeric vector}
    \item{\code{Spine}}{a numeric vector}
    \item{\code{Width}}{a numeric vector}
    \item{\code{Weight}}{a numeric vector}
    \item{\code{Satellites}}{a numeric vector}
  }
}
\details{
  Color is coded as 1 (light medium), 2 (medium), 3 (dark medium), 4
  (dark).  Spine condition is coded as 1 (both good), 2 (one worn or
  broken), 3 (both worn or broken).
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 3.2.  
}
\references{
  H. Jane Brockmann (1996). Satellite Male Groups in Horseshoe Crabs,
  Limulus polyphemus. _Ethology_ 102:1, 1--21.
}
\examples{
data(horseshoecrabs)
summary(glm(Satellites ~ Width, data=horseshoecrabs, family=poisson))
}
\keyword{datasets}
