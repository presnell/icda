\name{smokinglungcancer}
\alias{smokinglungcancer}
\docType{data}
\title{ Smoking and Lung Cancer in China }
\description{
  Data from eight studies in China about smoking and lung cancer.
}
\usage{data(smokinglungcancer)}
\format{
  The format is:
 'table' int [1:8, 1:2, 1:2] 126 908 913 235 402 182 60 104 100 688 ...
 - attr(*, "dimnames")=List of 3
  ..$ City      : chr [1:8] "Beijing" "Shanghai" "Shenyang" "Nanjing" ...
  ..$ LungCancer: chr [1:2] "Yes" "No"
  ..$ Smoking   : chr [1:2] "Yes" "No"
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 5.12.
}
\references{
  Z. Liu (1992). Smoking and Lung Cancer in China: Combined Analysis of
  Eight Case-Control Studies.  \emph{Int. J. Epidemiol.}, 21:2,
  197--201.
}
\examples{
data(smokinglungcancer)
ftable(LungCancer ~ City + Smoking, data=smokinglungcancer)
slc <- as.data.frame(smokinglungcancer)
library(reshape2)
slc <- dcast(slc, City + Smoking ~ LungCancer)
names(slc)[3:4] <- c("Cancer","NoCancer")
slc <- transform(slc, Smoking = relevel(Smoking, "No"))
fit1 <-
  glm(cbind(Cancer,NoCancer) ~ City + Smoking, family=binomial, data=slc)
summary(fit1)
}
\keyword{datasets}
