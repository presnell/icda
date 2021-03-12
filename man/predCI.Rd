\name{predCI}
\alias{predCI}
\alias{predCI.default}
\alias{predCI.glm}
\title{Confidence intervals for predicted mean values.}
\description{
  A convenience function that computes confidence intervals for
  predicted mean values for a fitted generalized linear model by first
  computing a Wald interval for the linear predictor and then applying
  the inverse link function to the endpoints of the interval.  For an lm
  object this is nothing more than a wrapper for predict.lm.
}
\usage{
predCI(object, ...)

\method{predCI}{default}(object, level = 0.95, ...)
\method{predCI}{glm}(object, level = 0.95, ...)
}
\arguments{
  \item{object}{
    a fitted a fitted object of class inheriting from "glm" or "lm"
}
  \item{level}{
    confidence level for interval
}
  \item{\dots}{
    arguments other than "level", "type" or "se.fit" to be passed to
    predict.lm or predict.glm
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Brett Presnell}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(horseshoecrabs)
hscrabs.logit <-
  glm((Satellites > 0) ~ Weight, family=binomial, data=horseshoecrabs)
predCI(hscrabs.logit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
