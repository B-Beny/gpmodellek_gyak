nonlinear_regression = function(x, y) {
  if (length(x) != length(y)) {
    return("X and Y must be the same length")
  }

  y_log <- log(y)

  n <- length(x)
  
  mean_x <- mean(x)
  mean_y_log <- mean(y_log)

  sum1 <- sum((x - mean_x) * (y_log - mean_y_log))
  sum2 <- sum((x - mean_x)^2)

  b <- sum1 / sum2
  a_log <- mean_y_log - b * mean_x

  a <- exp(a_log)

  cat("Value of a: ", a, "\n")
  cat("Value of b: ", b, "\n")

  plot(x, y, main = "Nonlinear Regression: y = ae^(bx)")
  curve(a * exp(b * x), add = TRUE, col = "blue", lwd = 2)
}

# Example usage
set.seed(42)
a_true <- 3
b_true <- 0.5
x <- seq(0, 10, length.out = 100)
y <- a_true * exp(b_true * x) + rnorm(length(x), sd = 0.5)

nonlinear_regression(x, y)