\name{UFAdmissions}
\alias{UFAdmissions}
\docType{data}
\title{ Graduate Admissions at the University of Florida }
\description{
  Results of graduate school applications to the 23 departments in the
  College of Liberal Arts and Sciences at the University of Florida
  during the 1997--98 academic year.  Applications are cross-classified
  according to department, the applicant's gender, and whether the
  applicant was admitted.
}
\usage{data(UFAdmissions)}
\format{
  A data frame with 92 observations on the following 4 variables.
  \describe{
    \item{\code{Dept}}{a factor with levels \code{anth} \code{astr} \code{chem} \code{clas} \code{comm} \code{comp} \code{engl} \code{geog} \code{geol} \code{germ} \code{hist} \code{lati} \code{ling} \code{math} \code{phil} \code{phys} \code{poli} \code{psyc} \code{reli} \code{roma} \code{soci} \code{stat} \code{zool}}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Decision}}{a factor with levels \code{Admitted} \code{Rejected}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\source{
  Alan Agresti (2007) _An Introduction to Categorical Data Analysis
  (Second Edition)_ Table 5.5.
}
\examples{
data(UFAdmissions)
library(reshape2)
UFw <- dcast(UFAdmissions, Dept + Gender ~ Decision, value.var="Freq")
head(UFw)
UF.fit <- glm(cbind(Admitted,Rejected) ~ Dept, family=binomial, data=UFw)
rstandard(UF.fit)
}
\keyword{datasets}
