tpxg.mle <- function(x) {

  log_lik <- function(params, x, n, x_2, sx) {
    alpha <- exp( params[1] )
    theta <- exp( params[2] )
    -2 * n * log( theta ) + n * log( alpha + theta ) - sum( log1p( alpha * theta * x_2 / 2 ) ) + theta * sx
  }

  x_2 <- x^2
  sx <- sum(x)
  n <- length(x)
  result <- optim(par = rnorm(2), fn = log_lik, x = x, n = n, x_2 = x_2, sx = sx, control = list(maxit = 10000) )
  c( alpha = exp( result$par[1] ), theta = exp( result$par[2] ), 'loglik' = -result$value )
}
