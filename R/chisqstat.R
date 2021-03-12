chisqstat <- function(object) {
  with(object,
       sum(prior.weights*(y - fitted.values)^2 /
           family$variance(fitted.values)))
}
