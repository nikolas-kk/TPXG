tppxg.reg <- function(y, x) {

  x <- model.matrix( y~., as.data.frame(x) )
  p <- dim(x)[2]

  log_lik <- function(params, x, y,z1, z2, z3) {
    a <- exp(params[1])
    b <- params[-1]
    mu <- as.vector( x %*% b )
    part1 <- exp(mu)
    part2 <- 1 - a * part1
    part3 <- sqrt( (a * part1 - 1)^2 + 12 * a * part1 )
    f1 <- 2 * log(part2 + part3) + z1 + y * mu
    f2 <- z3 * log(2 * part1 + part2 + part3)
    f3 <- -log1p( a * part1 + part3)
    f4 <- ( 2 * part1 + part2 + part3 )^2
    f5 <- a * part1 * ( part2 + part3 ) * z2
    - sum( f1 + f2 + f3 + log(f4 + f5) )
  }

  z1 <- y * log(2)
  z2 <- (y + 1) * (y + 2)
  z3 <- - y - 3
  start <- c( rnorm(1), numeric(p) )
  result <- optim(start, fn = log_lik, x = x, y = y,z1 = z1, z2 = z2, z3 = z3, control = list(maxit = 10000) )
  a <- result$par[1]
  b <- result$par[-1]
  list( alpha = exp(a), beta = b, "loglik" = -result$value)
}
