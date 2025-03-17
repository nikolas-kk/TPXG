rtpxg <- function(n, alpha = 1, theta = 1) {
  x <- numeric(n)                    
  cond <- theta / (alpha + theta)     
  q <- Rfast2::Runif(n)
  exp_ind <- q <= cond
  size1 <- sum(exp_ind)
  x[exp_ind] <- rexp(size1, theta)
  x[!exp_ind] <- rgamma(n - size1, 3, theta)
  x
}