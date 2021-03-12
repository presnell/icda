\name{ideology}
\alias{ideology}
\docType{data}
\title{ Political Ideology and Party Affiliation }
\description{
  Data from the General Social Survey cross-classifying respondents by
  gender, party affiliation, and political ideology.
}
\usage{data(ideology)}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{Party}}{a factor with levels \code{Dem} \code{Rep}}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Ideology}}{a factor with levels \code{VLib} \code{SLib} \code{Mod} \code{SCon} \code{VCon}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 6.7.
}
\examples{
data(ideology)
ftable(xtabs(Freq ~ Gender + Party + Ideology, data=ideology),
       row.vars=c("Gender","Party"))
library(reshape2)
ideow <- dcast(ideology, Gender + Party ~ Ideology, value.var="Freq")
library(VGAM)
ideo.cl1 <-
   vglm(cbind(VLib,SLib,Mod,SCon,VCon) ~ Party,
        family=cumulative(parallel=TRUE), data=ideow)
summary(ideo.cl1)
## Note deviance different from Table 6.8:
deviance(ideo.cl1)
df.residual(ideo.cl1)
## Deviance in text computed against marginal table ignoring Gender:
ideow.ng <- dcast(ideology, Party ~ Ideology, sum, value.var="Freq")
ideo.cl1.alt <-
   vglm(cbind(VLib,SLib,Mod,SCon,VCon) ~ Party,
        family=cumulative(parallel=TRUE), data=ideow.ng)
deviance(ideo.cl1.alt)
df.residual(ideo.cl1.alt)
}
\keyword{datasets}
