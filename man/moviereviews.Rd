\name{moviereviews}
\alias{moviereviews}
\docType{data}
\title{ Movie Reviewers }
\description{
  Cross-tabulation of reviews of 160 movies by film critics Gene
  Siskel and Roger Ebert from April 1995 through September 1996.
}
\usage{data(moviereviews)}
\format{
  The format is:
 table [1:3, 1:3] 24 8 10 8 13 9 13 11 64
 - attr(*, "dimnames")=List of 2
  ..$ Siskel: chr [1:3] "Con" "Mixed" "Pro"
  ..$ Ebert : chr [1:3] "Con" "Mixed" "Pro"
}
\source{
  Alan Agresti (2002) _Categorical Data Analysis (Second Edition)_ Table
  7.1.
}
\references{
  Alan Agresti and Lawrence Winner (1997). Evaluating Agreement and
  Disagreement Among Movie Reviewers.  _Chance_ 10, 10--14.
}
\examples{
data(moviereviews)
## maybe str(moviereviews) ; plot(moviereviews) ...
}
\keyword{datasets}
