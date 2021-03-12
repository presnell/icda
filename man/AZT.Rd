\name{AZT}
\alias{AZT}
\docType{data}
\title{ AZT Use and AIDS }
\description{
  Development of AIDS symptoms by AZT use and race.
}
\usage{data(AZT)}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{Symptoms}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{AZTUse}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{Race}}{a factor with levels \code{White} \code{Black}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\details{
  Data from a study of 338 veterans whose immune systems were beginning
  to falter after infection with the AIDS virus.  Subjects were randomly
  assigned to either receive AZT immediately and or only after their T
  cells showed severe immune weakness. Subjects were then observed to see
  whether they developed AIDS symptoms during the 3-year study.
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 4.4.  
}
\references{
  _New York Times_ (February 15, 1991).
}
\examples{
data(AZT)
ftable(xtabs(Freq ~ AZTUse + Race + Symptoms, data=AZT),
       row.vars=c("Race","AZTUse"))
library(reshape2)
AZTw <- dcast(AZT, AZTUse + Race ~ Symptoms, value.var="Freq")
AZTw
AZTw <- transform(AZTw,
                  AZTUse=relevel(AZTUse,"No"),
                  Race=relevel(Race,"Black"))
AZT.fit1 <- glm(cbind(Yes,No) ~ AZTUse + Race, family=binomial, data=AZTw)
summary(AZT.fit1)
chisqstat(AZT.fit1)
}
\keyword{datasets}
