\name{depression}
\alias{depression}
\docType{data}
\title{ Longitudinal Study of Treatments for Depression }
\description{
  Level of depression measured after 1, 2, and 4 weeks of treatment for
  each of 340 subjects.
}
\usage{data(depression)}
\format{
  A data frame with 1020 observations on the following 5 variables.
  \describe{
    \item{\code{subject}}{subject id}
    \item{\code{severity}}{a factor with levels \code{mild} \code{severe}}
    \item{\code{drug}}{a factor with levels \code{std} \code{new}}
    \item{\code{time}}{a numeric vector, log2(weeks on treatment)}
    \item{\code{response}}{a factor with levels \code{abnormal} \code{normal}}
  }
}
\details{
  Data from a hypothetical study in which subjects were initially
  classified according to whether their depression was mild or severe.
  Within each group, subjects were randomized to receive either a
  standard drug or a new drug for treatment of depression.  Following 1
  week, 2 weeks, and 4 weeks of treatment, each subject's level of
  mental depression was classified as normal or abnormal.
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 9.1.
}
\references{
  Gary G. Koch, J. Richard Landis, Jean L. Freeman, Daniel H. Freeman,
  Jr., Robert G. Lehnen (1977). A General Methodology for the Analysis
  of Experiments with Repeated Measurement of Categorical
  Data. \emph{Biometrics} 33:1, 133--158.
}
\examples{
library(icda)
library(gee)
library(reshape2)
data(depression)
depr.fit1 <-
  gee((response == "normal") ~ severity + drug*time, id=subject,
      data=depression, family=binomial)
summary(depr.fit1)
## I cannot figure out how to do this simple thing using the
## reshape2 package.  So here it is with R's reshape() function.
deprw <- reshape(depression, timevar="time", idvar="subject",
                 v.names="response", direction="wide")
names(deprw)
names(deprw) <- gsub("response.", "resp", names(deprw))
levels(deprw$resp0)
levels(deprw$resp0) <- levels(deprw$resp1) <- levels(deprw$resp2) <- c("A","N")
head(deprw)
ftable(xtabs(~ severity + drug + resp0 + resp1 + resp2, deprw),
       row.vars=c("severity","drug"))
## Here's a slick way to get Table 9.2:
acast(depression, severity + drug ~ time, value.var="response", function(x) mean(x=="normal"))
## How to fit the model for Problem 9.4:
depression <- transform(depression, weeks = 2^time)
depr.fit2 <-
  gee((response == "normal") ~ severity + drug*weeks, id=subject,
      data=depression, corstr="exchangeable", family=binomial)
summary(depr.fit2)
}
\keyword{datasets}
