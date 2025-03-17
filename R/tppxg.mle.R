tppxg.mle <- function(x) {

  log_lik <- function(params, x, n, z1, z2) {
    alpha <- exp(params[1])
    theta <- exp(params[2])
    -2 * n * log(theta) + n * log(alpha + theta) + z1 * log1p(theta) -
     sum( log( (1 + theta)^2 + (alpha * theta * z2) ) )
  }

  n <- length(x)
  z1 <- sum(x) + 3 * n
  z2 <- 0.5 * (x + 1) * (x + 2)
  result <- optim( rnorm(2), fn = log_lik, x = x, n = n, z1 = z1, z2 = z2, control = list(maxit = 10000) )
  c( alpha = exp(result$par[1]), theta = exp(result$par[2]), 'loglik' = -result$value )
}
