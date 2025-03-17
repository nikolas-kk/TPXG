ptpxg <- function(x, alpha = 1, theta = 1) {
  1 - ( ( ( alpha + theta + alpha * theta * x + (alpha * theta^2 * x^2 / 2) ) ) / (alpha + theta) ) * exp(-theta * x)
}
