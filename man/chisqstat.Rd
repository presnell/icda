\name{chisqstat}
\alias{chisqstat}
\title{ Chi-square Statistic }
\description{Compute Pearson's chi-square goodness-of-fit statistic for
  a fitted generalized linear model (glm object).}
\usage{
chisqstat(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ a fitted glm model }
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
data(deathpenalty)
library(reshape2)
dpwide <- dcast(deathpenalty, Defendant + Victim ~ DeathPenalty)
dp.fit <-
  glm(cbind(Yes,No) ~ Defendant + Victim, family=binomial,
      data=dpwide)
chisqstat(dp.fit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
