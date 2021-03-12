\name{cohens.kappa}
\alias{cohens.kappa}
\title{ Cohen's Kappa }
\description{ Computes Cohen's Kappa for a square contingency table. }
\usage{
cohens.kappa(x)
}
\arguments{
  \item{x}{
    a square contingency table
}
}
\details{
  Computes Cohen's kappa and its standard error for two raters with
  ratings presented in a square contingency table with rows
  corresponding to the ratings of the first rater and columns the
  ratings of the second.
}
\value{
  \item{comp1 }{The value of Cohen's kappa for the table.}
  \item{comp2 }{The standard error of kappa.}
}
\references{
  Alan Agresti (2002) \emph{Categorical Data Analysis} (2nd
  edition). Wiley.

  Alan Agresti (2007), \emph{An Introduction to Categorical Data
    Analysis} (2nd edition). Wiley.
}
\author{
  Brett Presnell
}
\note{
  Several R packages provide more general versions of Cohen's
  kappa with additional functionality.
}
\examples{
data(moviereviews)
cohens.kappa(moviereviews)
}
