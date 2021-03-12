\name{MBdrink}
\alias{MBdrink}
\docType{data}
\title{Drinking and the Meyers-Briggs personality test}
\description{
  Data on drinking frequently and four scales of the Meyers-Briggs
  personality test.
}
\usage{data(MBdrink)}
\format{
  A data frame with 32 observations on the following 6 variables.
  \describe{
    \item{\code{EI}}{a factor with levels \code{E} \code{I}}
    \item{\code{SN}}{a factor with levels \code{S} \code{N}}
    \item{\code{TF}}{a factor with levels \code{T} \code{F}}
    \item{\code{JP}}{a factor with levels \code{J} \code{P}}
    \item{\code{Drink}}{a factor with levels  \code{Rarely}} \code{Often}
    \item{\code{Count}}{a numeric vector}
  }
}
\details{
  Cross classification of a sample of people from the MBTI Step II
  National Sample on whether they report drinking frequently and on the
  four binary scales of the Myers-Briggs personality test:
  Extroversion/Introversion (E/I), Sensing/iNtuitive (S/N),
  Thinking/Feeling (T/F), and Judging/Perceiving (J/P).
}
\source{
  MBTI Step II National Sample (collected and compiled by CPP, Inc.,
  Mountain View, CA 94043)
}
\references{
  Alan Agresti, \emph{An Introduction to Categorical Data Analysis, 2nd
    ed.}, Wiley, (2007), Table 4.13.
}
\examples{
data(MBdrink)
## Table 4.13
ftable(xtabs(Count ~  JP + Drink + TF + SN + EI, data=MBdrink),
       row.vars=c("TF", "JP"), col.vars=c("EI","SN","Drink"))
## Problem 4.16.  Need data in wide format.
library(reshape2)
MBdrinkm <- melt(MBdrink, id=1:5, measurement=6)
MBdrinkw <- dcast(MBdrinkm, ... ~ Drink)
MBdrink.fit <- glm(cbind(Often,Rarely) ~ EI + SN + TF + JP,
                   family=binomial, data=MBdrinkw)
## Problem 7.6 uses these same data collapsed across Drink.
MB <- dcast(MBdrinkm, JP + TF + SN + EI ~ variable, sum)
## Table 7.21
ftable(xtabs(Count ~  JP + TF + SN + EI, data=MB),
       row.vars="JP", col.vars=c("EI","SN","TF"))
## Problem 7.6(b)
llha <- glm(Count ~  JP*TF + JP*SN + JP*EI + TF*SN + TF*EI  + SN*EI,
            family = poisson, data=MB)
## Problem 7.7
llci <- glm(Count ~  JP*TF + JP*SN + TF*SN + SN*EI,
            family = poisson, data=MB)
## To get the same signs as the text:
MB2 <- transform(MB, JP=relevel(JP,"P"), EI=relevel(EI,"I"))
llci2 <- glm(Count ~  JP*TF + JP*SN + TF*SN + SN*EI,
             family = poisson, data=MB2)
}
\keyword{datasets}
