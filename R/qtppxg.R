qtppxg <- function(p, alpha = 1, theta = 1, tol = 1e-5) {
  n <- length(p)
  x0 <- rep(1 / theta, n)
  den <- 2 * (alpha + theta)
  k <- 1 - p
  c1 <- theta + 1
  c2 <- log(c1)
  c3 <- alpha * theta
  c4 <- c3 * theta
  c5 <- 2 * theta * c1^2 + 2 * alpha
  repeat {
    c6 <- (x0^2 + 5 * x0 + 6) * c4 + 2 * (x0 + 3) * c3 + c5
    c7 <- c1^(-x0 - 3)
    g <- k - c6 / ( den * c1^(x0 + 3) )
    dg <- ( c7 * c2 * c6 - c7 * (c4 * (2 * x0 + 5) + 2 * c3) ) / den
    x1 <- x0 - g / dg
    if ( max( abs(x1 - x0) ) < tol )
      break
    x0 <- x1
  }
  x0
}
