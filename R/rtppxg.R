rtppxg <- function(n, alpha = 1, theta = 1) { 
  lam <- rtpxg(n,alpha,theta)
  rpois(n, lam)
}