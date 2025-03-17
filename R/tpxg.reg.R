tpxg.reg <- function(y, x) {
  x <- model.matrix( y~., as.data.frame(x) )

  p <- dim(x)[2]
  log_lik <- function(params, x, y, n, y_2) {
    a <- exp( params[1] )
    b <- params[-1]
    k <- x %*% b
    z <- exp(k)
    sum( 2 * k - log(a + z) + log1p(a * z * y_2 / 2) - z * y)
  }

  y_2 <- y^2
  n <- length(y)
  start <- c( rnorm(1), numeric(p) )
  result <- optim(start, fn = log_lik, x = x, y = y, y_2 = y_2 , n=n ,control = list(maxit = 10000) )
  a <- result$par[1]
  b <- result$par[-1]
  list(alpha = exp(a), beta = b, "loglik" = -result$value )
}
