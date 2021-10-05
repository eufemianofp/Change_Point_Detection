# Change Point Detection demo
R Shiny application to showcase an implementation of a change point detection 
algorithm for the early detection of rising failure rates of products.

Part of this project has been heavily influenced by the post from Marc Lavielle 
about [Detection of change points in a time series](http://sia.webpopix.org/changePoints.html). 


## Methods

1. **Detection of change points**. The number of change points is tested from 1 to 
10 (in the automatic selection mode), and for each number of change points the 
optimal change points are found and the Residual Sum of Squares (RSS) is 
recorded. The optimal number of change points is selected by finding the elbow 
in the RSS curve (done with file findElbow.R by Bryan A. Hanson). 

2. **Computation of performance**. In this project, the performance is understood 
as how likely it is that the failure rate of a product has actually gone up, as 
opposed to it being random chance. The higher the likelihood of the failure 
rate to have actually gone up, the lower the product is performing. The 
performance is calculated by using bootstrapping and Welch's t-test to get a 
p-value of how likely it is that the failure rate has gone up, then this 
p-value is mapped to a value (performance) between 0% and 100% to help people 
without knowledge of p-values interpret the performance of a product line.


## Files

- app.R: Shiny app.
- dynProg.R: function that solves the dynamic programming problem to find the 
optimal change points.
- findElbow.R: function to find the elbow of the RSS curve.
- performance.R: function to map the p-value to a value between 0 and 100%. The 
sensitivity parameter selects a specific mapping among 10 possible mappings. 
The higher the sensitivity, the lower the performance for a fixed p-value.
- performance_functions.R: function to determine the coefficients of the 10 
sensitivity mappings in performance.R.


## Implementation

Download the project and open it in RStudio. Then run the file app.R to run the 
Shiny application.

