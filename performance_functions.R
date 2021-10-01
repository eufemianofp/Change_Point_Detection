
rm(list = ls())
library("nleqslv")

sensitivity_levels <- 10
A <- matrix(nrow = sensitivity_levels, 
            ncol = 3)

for (i in 1:sensitivity_levels) {

  ## Boundary conditions
  x_s <- 0.3  # significant x, lost meaning when applying flexible sensitivity
  b <- 20
  sensitivity <- i
  y_s <- 1 - 10 ^ (-b / sensitivity)
  x_c <- 0.7  # x critical (change between exponential function and constant 1 function)
  
  ## Define system of nonlinear equations to solve (in the form f[i] = 0, plug y[i] <- f[i])
  fn <- function(a) {
    y <- numeric(3)
    y[1] <- 1 - a[1] - a[3]
    y[2] <- a[1] * exp(-a[2] * x_c) + a[3]
    y[3] <- a[1] * exp(-a[2] * x_s) + a[3] - (1 - y_s)
    return(y)
  }
  
  ## Solve the coefficients a[i] for given boundary conditions
  guess_a <- c(1, 1, 1)
  a <- nleqslv(x = guess_a, fn = fn)$x
  
  A[i, ] <- a
}


## Plot function
x1 <- seq(0, x_c, by = 0.001)
y1 <- 1 - (a[1] * exp(-a[2] * x1) + a[3])
x2 <- seq(x_c, 1, by = 0.001)
y2 <- rep(1, times = length(x2))

plot(x1, y1, type = "l",
     xlim = c(0, 1), ylim = c(0, 1),
     ylab = "Performance indicator", xlab = "p-value")
lines(x2, y2)

