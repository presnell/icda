\name{snoring}
\alias{snoring}
\docType{data}
\title{Snoring and Heart Disease}
\description{
  Frequency of heart disease among 2484 subjects classified according to
  snoring level as reported by their spouses.
}
\usage{data(snoring)}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{snoring}}{a factor with levels \code{Never} \code{Occassional} \code{Usually} \code{Always}}
    \item{\code{heartdisease}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007), _An Introduction to Categorical Data Analysis
  (Second Edition)_, Table 3.1, p. 69.
}
\references{
  P. G. Norton and E. V. Dunn, _Br. Med. J._, 291: 630--632, 1991.
}
\examples{
data(snoring)
library(reshape2)
snoringw <- dcast(snoring, snoring ~ heartdisease, value.var = "Freq")
names(snoringw)
with(snoringw, round(Yes/(Yes+No), 3))
snoringw$snorescore <- c(0, 2, 4, 5)
snoring.lin <-
  glm(cbind(Yes,No) ~ snorescore,
      family=binomial(make.link("identity")), data=snoringw)
coef(snoring.lin)
predict(snoring.lin, type="response")
}
\keyword{datasets}
