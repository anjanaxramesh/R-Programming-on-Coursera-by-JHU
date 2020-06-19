make.power <- function(n) {
  pow <- function(x){
    x^n
  }
  pow
}
cube <- make.power(3)
square <- make.power(2)

#y <- 10
f <- function(x) {
  y <- 2
  y^2 + g(x)
}
#g <- function(x) {
  x*y
}

g <- function(x) {
  a <- 3
  x + a + y
}

cube <- function(x, n) {
  x^3
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}