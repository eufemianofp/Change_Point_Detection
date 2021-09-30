
# Family of performance functions. Each row is a set of coefficients for one 
# function, which maps the p-value from the statistical test to a value between
# 0 and 100
A <- matrix(data = c(1.000000, 62.16834, 5.360223e-18,
                     1.000000, 62.16834, 5.146427e-18,
                     1.000000, 51.08439, -2.445734e-16,
                     1.000000, 38.37583, -2.153184e-12,
                     1.000000, 30.70112, -4.641622e-10,
                     1.000000, 25.58416, -1.668240e-08,
                     1.000000, 21.92886, -2.155212e-07,
                     1.000001, 19.18666, -1.469383e-06,
                     1.000007, 17.05257, -6.545097e-06,
                     1.000022, 15.34343, -2.165273e-05), 
            ncol = 3, 
            byrow = TRUE)

performance <- function (pvalue, sensitivity = 5) {
  
  # Pick coefficients based on selected sensitivity. Values range from 1 to 10.
  # The higher the sensitivity, the lower the performance score for a specific
  # p-value
  a <- A[sensitivity, ]

  if (pvalue < 0.7) {
    perf <- 1 - (a[1] * exp(-a[2] * pvalue) + a[3])
  } else {
    perf <- 1
  }

  return(perf)
}









