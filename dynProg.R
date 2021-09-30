

## Helper function to find change points
dynProg.mean <- function(y, Kmax, Lmin = 1) {
  Nr  <- Kmax - 1
  n <- length(y)
  V <- matrix(Inf, nrow = n, ncol = n)
  for (j1 in (1:(n - Lmin + 1))) {
    for (j2 in ((j1 + Lmin - 1):n)) {
      yj <- y[j1:j2]
      nj <- j2 - j1 + 1
      V[j1, j2] <- sum(yj ^ 2) - (sum(yj) ^ 2) / nj
    }
  }
  
  U <- vector(length = Kmax)
  U[1] <- V[1, n]
  D <- V[, n]
  
  Pos <- matrix(nrow = n, ncol = Nr)
  Pos[n,] <- rep(n, Nr)
  tau.mat <- matrix(nrow = Nr, ncol = Nr)
  
  for (k in 1:Nr) {
    for (j in 1:(n - 1)) {
      dist <- V[j, j:(n - 1)] + D[(j + 1):n]
      D[j] <- min(dist)
      Pos[j, 1] <- which.min(dist) + j
      
      if (k > 1) {
        Pos[j, 2:k] <- Pos[Pos[j, 1], 1:(k - 1)]
      }
    }
    U[k + 1] <- D[1]
    tau.mat[k, 1:k] <- Pos[1, 1:k] - 1
  }
  
  out <- list(Test = tau.mat, obj = data.frame(K = (1:Kmax), U = U))
  return(out)
}