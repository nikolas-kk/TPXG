qtpxg <- function(p, alpha = 1, theta = 1, tol = 1e-5){
  n <- length(p)
  x0 <- rep(1 / theta, n)
  den <- alpha + theta
  z1 <- 1 - p
  z2 <- alpha * theta
  z3 <- z2 * theta / 2
  z4 <- theta^2
  ## This is fucking awesome
  repeat {
    k <- exp(-theta * x0)
    g <- z1 - (den + z2 * x0 + z3 * x0^2) / den * k
    dg <- k * z4 * (2 + z2 * x0^2) / (2 * den)
    x1 <- x0 - g / dg
    if ( max( abs(x1 - x0) ) < tol )
      break
    x0 <- x1
  }
  x0
}
