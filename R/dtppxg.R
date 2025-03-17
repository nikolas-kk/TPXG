dtppxg <- function(x, alpha = 1, theta = 1) {
  part1 <- theta^2 / ((alpha + theta) * (1 + theta)^(x + 3))
  part2 <- (1 + theta)^2 + alpha * theta * (x + 1) * (x + 2) / 2
  part1 * part2
}