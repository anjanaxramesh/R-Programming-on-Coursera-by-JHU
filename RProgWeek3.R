x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)

# group means
tapply(x, f, mean)

# group means without simplification
tapply(x, f, mean, simplify = FALSE)

# group ranges
tapply(x, f, range)
