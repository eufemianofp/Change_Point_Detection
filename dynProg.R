#' @title
#' Find change points in a time series
#'
#' @description
#' This method finds change points in a time series that has different segments
#' with different means. The point returned as change point is the point before
#' a new segment starts, i.e. the last point of a segment before the next one
#' starts.
#'
#' @param y Numeric vector of y values of time series
#' @param max_ncp Maximum number of change points to consider
#' @param Lmin Minimum length (number of points) of a segment
#'
#' @return List with two objects. The first object is a dataframe with the
#'         row number being the number of change points considered and the
#'         values in each row being the data position before the change
#'         happened. The second object contains the Residual Sum of Squares
#'         (named U) for different number of change points from 1 to Kmax - 1
#'
#' @references
#' The concept of this function is based on a dynamic programming approach to 
#' find change points in a time series explained by Marc Lavielle in the course
#' Statistics in Action at the Ecole Polytechnique posted at
#' http://sia.webpopix.org/changePoints.html. The code of this function is a
#' modified version of the one posted on that site.
#' 
#' @author Eufemiano Fuentes Perez. \email{eufe.f.p@@gmail.com}
#'
#' @export
#'
dynProg.mean <- function(y, max_ncp, Lmin = 1) {
  
  Kmax <- max_ncp + 1
  n <- length(y)
  V <- matrix(Inf, nrow = n, ncol = n)
  for (j1 in 1:(n - Lmin + 1)) {
    for (j2 in (j1 + Lmin - 1):n) {
      yj <- y[j1:j2]
      nj <- j2 - j1 + 1
      V[j1, j2] <- sum(yj ^ 2) - (sum(yj) ^ 2) / nj
    }
  }
  
  U <- vector(length = Kmax)
  U[1] <- V[1, n]
  D <- V[, n]
  
  Pos <- matrix(nrow = n, ncol = max_ncp)
  Pos[n, ] <- rep(n, max_ncp)
  tau.mat <- matrix(nrow = max_ncp, ncol = max_ncp)
  
  for (k in 1:max_ncp) {
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
  
  out <- list(cps_pos = tau.mat,  # Matrix with change point positions
              obj = data.frame(ncp = 0:(Kmax - 1),  # number of change points
                               U = U))              # Residual Sum of Squares
  return(out)
}
