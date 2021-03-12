\name{teratology}
\alias{teratology}
\docType{data}
\title{Low-Iron Rat Teratology Data}
\description{Results of an experiment examining the effect of
  iron-deficiency on fetal development in laboratory rats.}
\usage{data(teratology)}
\format{
  A data frame with 58 observations on the following 4 variables.
  \describe{
    \item{\code{N}}{litter size}
    \item{\code{R}}{number of dead fetuses }
    \item{\code{HB}}{mother's hemoglobin level}
    \item{\code{GRP}}{a factor giving the treatment group with levels \code{1} \code{2} \code{3} \code{4}}
  }
}
\details{
 These data from Moore and Tsiatis (1991) include a variable (HB) not given
 in Table 9.4 (p. 283) of Agresti (2007), but are otherwise the same.
 They are discussed and analyzed in sections 9.2.4 and 10.2.3 of Agresti (2007).
}
\source{
  Moore, D. F. and  Tsiatis, A. (1991)
  Robust Estimation of the Variance in Moment Methods for Extra-Binomial and Extra-Poisson Variation.
  \emph{Biometrics} \bold{47}, 383--401.
}
\references{
  Agresti, A. A. (2007)
  \emph{An Introduction to Categorical Data Analysis} (2nd edition).
  Wiley.

  Shepard, T. H., Mackler, B., and Finch, C. A. (1980)
  Reproductive studies in the iron-deficient rat.
  \emph{Teratology} \bold{22}, 329--334.
}
\examples{
data(teratology)

## Binomial model fit
terat.binom <-
  glm(cbind(R, N-R) ~ GRP, data = teratology, family = binomial)
summary(terat.binom)

## Adding an explicit index for litter for remaining analyses.
teratology$Litter <- as.factor(1:nrow(teratology))

## Need data in ungrouped (binary) format for GEE (???):
teratbnry <- teratology
teratbnry$N <- teratbnry$R <- NULL
teratbnry$Litter <- 1:nrow(teratology)
teratbnry <- teratbnry[rep(1:nrow(teratology), teratology$N),]
rownames(teratbnry) <- NULL  # cleaning up row names
teratbnry$Response <-
  with(teratology,
       unlist(apply(cbind(R, N-R), 1,
                    function(x) rep(c("Dead","Alive"), x))))
## GEE fit:
library(gee)
terat.gee <-
  gee((Response == "Dead") ~ GRP, id = Litter, data = teratbnry,
      family = binomial, corstr = "exchangeable")
summary(terat.gee)

## Mixed model fit (can use ungrouped or group binary data):
library(lme4)
## Using grouped data
terat.glmm <-
  glmer(cbind(R, N-R) ~ GRP + (1|Litter),
        data = teratology, family = binomial)
summary(terat.glmm)
## Using ungrouped binary data
terat.glmm <-
  glmer((Response == "Dead") ~ GRP + (1|Litter),
        data = teratbnry, family = binomial)
summary(terat.glmm)

### Agresti does not discuss the simple quasi-likelihood approach:
## terat.qbinom <-
##   <- glm(cbind(R, N-R) ~ GRP, data = teratology,
##          family = quasibinomial)
## summary(terat.qbinom)

### Nor beta-binomial models, but there seems to be a problem with
### fitting this model anyway:
## library(VGAM)
## terat.bbinom <-
##   vglm(cbind(R, N-R) ~ GRP, data = teratology, family = betabinomial)
## summary(terat.bbinom)

## Suppose we had indexed litters within treatment groups.  Should
## possibly add this to examples.  Use id = interaction(Group,Litter)
## in gee().
}

\keyword{datasets}
