oddsratio <- function(x, ...) UseMethod("oddsratio")

oddsratio.default <- function(x, increment = 0, ...)
{
  x <- x + increment
  (x[1,1] * x[2,2])/(x[1,2] * x[2,1])
}

oddsratio.formula <- function(formula, data=list(), ...)
{
  x <- xtabs(formula, data = data)
  oddsratio.default(x, ...)
}
