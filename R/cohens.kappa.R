cohens.kappa <- function(x) {
  n <- sum(x)
  p <- x/n
  pdiag <- diag(p)
  p1 <- apply(p, 1, sum)
  p2 <- apply(p, 2, sum)
  Po <- sum(diag(p))
  Pe <- sum(p1 * p2)
  kappa <- (Po - Pe)/(1 - Pe)
  SE2 <-
    (((1-Po)/(1-Pe)^2) *
     (Po + 2*(2*Po*Pe - sum(pdiag*(p1 + p2)))/(1-Pe)
      + (1-Po)*(sum(p * outer(p1, p2, "+")^2) - 4*Pe^2)/(1-Pe)^2))/n
  list(kappa=kappa, SE=sqrt(SE2))
}
