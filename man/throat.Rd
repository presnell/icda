\name{throat}
\alias{throat}
\docType{data}
\title{ Sore Throat after Surgery }
\description{
  Duration of surgery, type of device used to secure the airway, and
  occurrence of sore throat for 35 patients after surgery under general
  anesthesia.
}
\usage{data(throat)}
\format{
  A data frame with 35 observations on the following 3 variables.
  \describe{
    \item{\code{D}}{a numeric vector}
    \item{\code{T}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
  }
}
\details{
  Duration of surgery (D) is given in minutes; the type of device used
  to secure the airway is coded as T=0 for laryngeal mask and T=1 for
  tracheal tube; occurrence of sore throat is coded as Y=1 (yes) and Y=0
  (no).
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 4.19.  
}
\references{
  D. Collett (1998) in _Encyclopedia of Biostatistics_, 350--358.
}
\examples{
data(throat)
summary(glm(Y ~ T + D, family=binomial, data=throat))
}
\keyword{datasets}
