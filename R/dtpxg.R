dtpxg <- function(x, alpha = 1, theta = 1) {
  (theta^2 / (alpha + theta)) * (1 + (alpha * theta * x^2 / 2)) * exp(-theta *x)
}