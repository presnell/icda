\name{psychdiagdrugs}
\alias{psychdiagdrugs}
\docType{data}
\title{ Psychiatric Diagnosis and Drugs Prescribed }
\description{
  Classification of psychiatric patients by diagnosis and whether
  drugs prescribed.
}
\usage{data(psychdiagdrugs)}
\format{
  A data frame with 10 rows and variables.
  \describe{
    \item{\code{Diagnosis}}{a factor with levels
      \code{Schizophrenia}
      \code{AffectiveDisorder}
      \code{Neurosis}
      \code{PersonalityDisorder}
      \code{SpecialSymptoms}
    }
    \item{\code{Drugs}}{a factor with levels \code{Y} \code{N}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 2.15 (Exercise 2.22), p. 61.
}
\references{
  E. Helmes and G. C. Fekken (1986).  Effects of psychotropic drugs and
  psychiatric illness on vocational aptitude and interest assessment.
  \emph{J. Clin. Psychol.}, 42:4, 569--576.
}
\examples{
library(icda)
data(psychdiagdrugs)
xtabs(Freq ~ Diagnosis + Drugs, data=psychdiagdrugs)
}
\keyword{datasets}
