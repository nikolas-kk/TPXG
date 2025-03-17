ptppxg <- function(x, alpha = 1, theta = 1) {
  part1 <- 1 / ( 2 * (alpha + theta) * (1 + theta)^(x + 3) )
  part2 <- (x^2 + 5 * x + 6) * alpha * theta^2 + 2 * (x + 3) * alpha * theta + 2 * theta * (1 + theta)^2 + 2 * alpha
  1 - part1 * part2
}
