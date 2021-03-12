\name{oddsratio}
\alias{oddsratio}
\alias{oddsratio.default}
\alias{oddsratio.formula}
\title{ Odds Ratio }
\description{Estimate the odds ratio for a 2 by 2 table of counts.}
\usage{
oddsratio(x, ...)

\method{oddsratio}{default}(x, increment = 0, ...)
\method{oddsratio}{formula}(formula, data=list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a 2 by 2 matrix of counts. }
  \item{increment}{ an increment to be added to the counts. }
  \item{formula}{ a formula to be passed to xtabs to create a 2 by 2
    table. }
  \item{data}{ an optional data frame containing the variables in the
    model. } 
  \item{\dots}{ not used. } 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
data(aspirin)
oddsratio(Freq~Group+MI, data=aspirin)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
