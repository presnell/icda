predCI <- function(object, ...) UseMethod("predCI")

predCI.default <- function(object, level=0.95, ...) {
  predict.lm(object, type="response", interval="confidence", ...)
}

predCI.glm <- function(object, level=0.95, ...) {
  z <- qnorm(level + (1-level)/2)
  res <-
    with(predict.glm(object, type="link", se.fit=TRUE, ...),
         cbind(fit = fit,
               lwr = fit - z*se.fit,
               upr = fit + z*se.fit))
  res <- family(object)$linkinv(res)
  res
}
  
