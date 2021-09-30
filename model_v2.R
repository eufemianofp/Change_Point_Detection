library(shiny)
library(ggplot2)

library(parallel)
library(doSNOW)
library(foreach)

source("findElbow.R")
source("performance.R")
source("dynProg.R")

## Maximum number of change points
maxNumberChangePoints <- 10


#### Shiny User Interface ####
ui <- fluidPage(
  
  h2("Change point detection"),
  
  plotOutput("plot1"),
  
  sliderInput(inputId = "burnin",
              label = "Burn-in size",
              value = 0,
              min = 0, max = 50),
  
  sliderInput(inputId = "last",
              label = "Remove last observations",
              value = 0,
              min = 0, max = 50),
  
  
  # plotOutput("plot2"),
  
  selectInput("changePointSelection",
              h4("How will you choose the change points?"), 
              choices = list("Automatically" = "automatic", 
                             "Semi-automatically" = "semi-automatic", 
                             "Manually" = "manual"), 
              selected = 1),
  
  
  # Only show this panel if "Automatically" is chosen
  conditionalPanel(
    condition = "input.changePointSelection == 'automatic'",
    p("Number of change points: "),
    verbatimTextOutput("nChangePoints")
  ),
  
  # Only show this panel if "Semi-automatically" is chosen
  conditionalPanel(
    condition = "input.changePointSelection == 'semi-automatic'",
    numericInput("nChangePoints", 
                 p("Choose number of change points: "), 
                 value = 1, 
                 min = 1, 
                 max = maxNumberChangePoints)
  ),
  
  # Only show this panel if "Manually" is chosen
  conditionalPanel(
    condition = "input.changePointSelection == 'manual'",
    radioButtons("add_remove_changePoint", label = "",
                 choices = list("Add change point" = "add", "Remove change point" = "remove"), 
                 selected = "add")
  ),
  
  # Only show this panel if "Automatically" or "Semi-automatically" is chosen
  conditionalPanel(
    condition = "input.changePointSelection == 'automatic' || input.changePointSelection == 'semi-automatic'",
    plotOutput("plot3"),
    p("Change points at: "), 
    verbatimTextOutput("plot3_text")
  ),
  
  # Only show this panel if "Manually" is chosen
  conditionalPanel(
    condition = "input.changePointSelection == 'manual'",
    plotOutput("plot3_manual", click = "plot3_click", hover = "plot3_hover"),
    fluidRow(
      column(width = 4, p("Pointer at: "), verbatimTextOutput("plot3_clickInfo")),
      column(width = 8, p("Change points at: "), verbatimTextOutput("plot3_text_manual"))
    )
  ),
  
  
  checkboxInput("regression_line", "Plot regression line", value = FALSE),
  
  br(),
  br(),
  
  actionButton("performance", "Compute performance"),
  
  br(),
  br(),
  
  plotOutput("hist"),
  
  br(),
  br(),
  
  conditionalPanel(
    condition = "input.performance > 0",
  
    sliderInput(inputId = "sensitivity",
                label = "Choose sensitivity",
                value = 5,
                min = 1, max = 10),
    verbatimTextOutput("performance"),
    
    checkboxInput("show_pvalue", "Show p-value", value = FALSE),
    conditionalPanel(
      condition = "input.show_pvalue == 1",
      verbatimTextOutput("show_pvalue")
    )
  ),
  
  br(),
  br(),
  br(),
  br()
  
)




#### Shiny Server ####

server <- function(input, output, session) {
  
  #### Data generation ####
  data_full <- reactive({
    set.seed(3)
    
    n1 <- 50
    n2 <- 50
    n <- n1 + n2
    
    mu1 <- 0.010
    mu2 <- 0
    sd <- 0.003
    
    # First n1 observations with mean mu1 and standard deviation sd
    y1 <- mu1 + rnorm(n = n1, mean = 0, sd = sd)
    
    # Next n2 observations with a mean with exponential drift and same standard
    # deviation
    intercept <- 0.001
    mu2 <- mu1 +  # same previous mean
           intercept * exp( (1:n2 + 30) * 0.03 ) - intercept  # exponential drift
    y2 <- mu2 + rnorm(n = n2, mean = 0, sd = sd)
    
    y1[y1 < 0] <- y2[y2 < 0] <- 0
    
    data.frame(time = 1:n,
               y = c(y1, y2))
  })
  
  ## Change default values in slider inputs
  observe({
    updateSliderInput(session, "burnin", value = 0)
    updateSliderInput(session, "last", value = 0)
  })
  
  #### Reactive variables ####
  
  ## Number of data points in full dataset
  n_full <- reactive({ nrow(data_full()) })
  
  ## Data filtered
  data <- reactive({
    data_full()[(1 + input$burnin) : (n_full() - input$last), ]
  })
  
  ## Number of observations after filtering
  n <- reactive({ length(data()$y) })
  
  ## Create list of reactive values, and create value cp for change points
  rv <- reactiveValues(cp = NULL)
  
  
  
  #### Plot 1 ####
  output$plot1 <- renderPlot({
    
    ggplot(data = data()) +  
      geom_point(aes(time, y), color="#6666CC") +
      xlab("Time (months)") +
      ylab("Failure rate (%)")
  })
  
  
  
  #### Find change points automatically ####
  r <- reactive({  # K is number of segments, add 5 so if elbow is in n=maxN it can be detected
    result <- dynProg.mean(data()$y, K=maxNumberChangePoints + 5)
    rv$nCPready <- TRUE
    result
    })  
  
  # ## PLOT 2
  # 
  # This is the plot that shows how likely are the different points to be a change point
  #
  # output$plot2 <- renderPlot({
  # 
  #   ggplot(data=r()$obj) +
  #     geom_line(aes(K,U), size=1, colour="purple") +
  #     geom_point(aes(K,U), size=2, colour="purple")
  # })
  
  
  
  # Get number of change points
  nChangePoints <- reactive({
    
    if (input$changePointSelection == "automatic"){  # Automatic
      findElbow(r()$obj$U) - 1
    } else {  # Semi-automatic and Manual
      as.numeric(input$nChangePoints)
    }
  })
  
  # Output number of change points
  output$nChangePoints <- renderText({ nChangePoints() })
  
  
  # Change points including 0 and n
  Topt <- reactive({ c(0, r()$Test[nChangePoints(), 1:nChangePoints()], n()) })
  
  # Separation between different segments (change points + 0.5)
  Tr <- reactive({ c(0, Topt()[2:(nChangePoints() + 1)] + 0.5, n()) + input$burnin })
  
  
  
  # This is to avoid an error due to lack of values when launching the app
  observeEvent(input$changePointSelection, { rv$cp <- Tr() }, once = TRUE)
  
  # Update every time we go back to manual if necessary
  observe({
    if (input$changePointSelection == "manual") { 
      if (rv$cp[length(rv$cp)] != n()) {
        rv$cp <- Tr()
      }
    }
  })
  
  
  
  # x_n of first mean, x_1 - 1 of second mean
  x1 <- reactive({
    if (input$changePointSelection == "manual") {
      rv$cp[length(rv$cp) - 1] + 0.5
    } else {
      Tr()[nChangePoints() + 1] + 0.5
    }
  })
  
  # Last data point in filtered data, x_2 of second mean
  x_end <- reactive({ n() + input$burnin })
  
  # x_1 of first mean
  x_previous <- reactive({
    if (input$changePointSelection == "manual") {
      rv$cp[length(rv$cp) - 2] + 0.5
    } else {
      Tr()[nChangePoints()] + 0.5
    }
  })
  
  
  
  #### Plot 3 ####
  
  # Data mean per segment, green horizontal lines in the plot (automatic and semi-automatic)
  dm <- reactive({
    
    d <- data.frame()
    
    # Store mean for each segment
    for (k in ( 1:(nChangePoints() + 1) )) {
      m <- mean( data()$y[(Topt()[k] + 1) : (Topt()[k + 1])] )
      d <- rbind(d, c(Tr()[k], Tr()[k + 1], m, m) )
    }
    names(d) <- c("x1","x2","y1","y2")
    d
  })
  
  # Data for the dotted regression line
  data_regression <- reactive({
    
    length_previous_segment <- abs( x1() - x_previous() )
    extra_lm <- floor(length_previous_segment * 0.1)  # take into account last 10% from previous interval (sometimes the change point is slightly after the trend started, this helps get the correct trend line)
    
    # x and y for the linear regression model
    x_lm <- (x1() - extra_lm) : x_end()
    y_lm <- data_full()$y[x_lm]
    
    fit <- lm(y ~ x, data = data.frame(x = x_lm, y = y_lm))
    # fit <- loess(y ~ x, data = data.frame(x = x_lm, y = data_full()$y[x_lm]), span = 10)
    
    x_plot <- seq(x1(), x_end(), by = 0.01)
    y_plot <- predict(fit, newdata=data.frame(x=x_plot))
    d <- data.frame(x_plot = x_plot, y_plot = y_plot)
    d
  })
  
  output$plot3 <- renderPlot({
    
    g <- ggplot(data()) +
      geom_point(aes(time, y), color="#6666CC") +
      ylab("failure rate") +
      geom_vline(xintercept = Tr()[2:(nChangePoints() + 1)], color="red", size=0.25) +
      geom_segment(data = dm(), aes(x=x1, y=y1, xend=x2, yend=y2), colour="green", size=0.75)
    
    if (input$regression_line){
      g <- g + geom_line(data = data_regression(), aes(x_plot, y_plot), colour="black", size=0.75, linetype = "dashed")
    }
    g
  })
  
  output$plot3_text <- renderText({ Tr()[2:(nChangePoints() + 1)] })
  
  
  
  # The following manual outputs are the same as the ones above, but it's the only way I found that 
  # R Shiny allowed to have a "clickable" plot only for the manual setting, and not clickable for
  # the automatic and semi-automatic mode
  
  output$plot3_manual <- renderPlot({
    
    # Update change points
    if (!is.null(input$plot3_click)) {
      # Get new change point
      x_new <- (ceiling(input$plot3_click$x) + floor(input$plot3_click$x)) / 2
      
      if (input$add_remove_changePoint == "add"){
        rv$cp <- sort(c(isolate(rv$cp), x_new))  # add new change point
      } else {
        if (x_new %in% rv$cp){  # if change point exists
          rv$cp <- rv$cp[-which(rv$cp == x_new)]  # remove change point
        }
      }
    }
    
    nSegments_new <- length(rv$cp) - 1
    Topt_new <- floor(rv$cp)
    
    # Data means (manual)
    d <- data.frame()
    for (k in 1:nSegments_new) {
      m <- mean( data()$y[(Topt_new[k] + 1) : (Topt_new[k + 1])] )
      d <- rbind(d, c(rv$cp[k], rv$cp[k + 1], m, m) )
    }
    names(d) <- c("x1","x2","y1","y2")
    
    g <- ggplot(data()) +
      geom_point(aes(time, y), color="#6666CC") +
      ylab("failure rate") +
      geom_vline(xintercept = rv$cp[2:nSegments_new], color="red", size=0.25) +
      geom_segment(data = d, aes(x=x1, y=y1, xend=x2, yend=y2), colour="green", size=0.75)
    
    if (input$regression_line) {
      g <- g + geom_line(data = data_regression(), aes(x_plot, y_plot), colour="black", size=0.75, linetype = "dashed")
    }
    g
  })
  
  output$plot3_clickInfo <- renderText({
    if (!is.null(input$plot3_hover)) {
      paste0("time = ", round(input$plot3_hover$x, 2), "\nfailure rate = ", round(input$plot3_hover$y, 4))
    } else {
      paste0("time = ", "\nfailure rate = ")
    }
  })
  
  output$plot3_text_manual <- renderText({ 
    rv$cp[2:(length(rv$cp) - 1)]
    # rv$cp  # also contains 0 and last data point
    })
  
  
  
  #### Compute Performance ####
  
  # So far the performance is only computed for the right-most change point (the last change point timewise)
  # This is something that can be changed easily, and we could even let the user choose which change
  # point to test. Furthermore, I only considered differences in mean that lead to an upward trend, 
  # so I take the minimum t value from the t-test (mu1 - mu2 is going to be negative is mu2 is larger)
  
  rv$V.hat <- NULL
  rv$V.stat <- NULL
  
  p_value <- eventReactive(input$performance, {
    
    y_perf <- data_full()$y[x_previous():x_end()]
    n_perf <- length(y_perf)
    
    ## Find change point assuming y's are normally distributed, using t.test for difference in means and picking largest statistic value S_t
    t.stat <- t.pval <- NULL
    for (tau in 1:(n_perf - 1)) {
      test.tau <- t.test(y_perf[1:tau], y_perf[(tau + 1):n_perf], var.equal = TRUE)
      t.stat <- c(t.stat, test.tau$statistic)
      t.pval <- c(t.pval, test.tau$p.value)
    }
    # tau.hat <- which.max(t.stat)  # for a difference in means in the opposite direction
    tau.hat <- which.min(t.stat)
    rv$V.hat <- t.stat[tau.hat]
    
    
    ## Register the parallel backend
    ncores <- detectCores() - 2
    cl <- makeCluster(ncores)
    registerDoSNOW(cl)
    
    n_sim <- 1000
    
    # The detail to be captured by the progress bar should be contained within this function and its braces
    withProgress(message = 'Computing performance', min = 0, max = n_sim, value = 0, {
      
      progress <- function(i) incProgress(1, detail = paste("Running simulation", i, "out of", n_sim))
      opts <- list(progress = progress)
      
      ## Run simulations in parallel
      rv$V.stat <- foreach(i=1:n_sim,
                           .combine = 'rbind',
                           .options.snow = opts,
                           .inorder = FALSE
      ) %dopar% {
        y.sim <- rnorm(n_perf)
        t.stat <- NULL
        for (tau in (1:(n_perf-1))) {
          test.tau <- t.test(y.sim[1:tau], y.sim[(tau+1):n_perf], var.equal = TRUE)
          t.stat <- c(t.stat, test.tau$statistic)
        }
        # tau.max <- which.max(t.stat)  # for a difference in means in the opposite direction
        tau.min <- which.min(t.stat)
        return(t.stat[tau.min])
      }
      stopCluster(cl)
    })
    
    # p_value <- 1 - ecdf(rv$V.stat)(rv$V.hat)  # for a difference in means in the opposite direction
    p_val <- ecdf(rv$V.stat)(rv$V.hat)
    p_val
  })
  
  
  output$hist <- renderPlot({
    
    if (!is.null(rv$V.stat)){
      
      ## Plot histogram of distribution of max(S_t)
      xmin <- floor(min(min(rv$V.stat) - 1, rv$V.hat - 1))
      hist(rv$V.stat, breaks = 50, probability = TRUE,
           xlim = c(xmin, max(rv$V.stat)),
           main = "Distribution of test statistic",
           xlab = "Test statistic value")
      abline(v = rv$V.hat, col = "darkred")
      
      # hist(rv$V.stat, breaks = 50, probability = TRUE, xlim = c(0, xmax))  # for a difference in means in the opposite direction
      # xmax <- ceiling(max(max(rv$V.stat) + 1, rv$V.hat + 1))  # for a difference in means in the opposite direction
      # abline(v = rv$V.hat, col = "darkred")  
    }
  })
  
  perf <- reactive({
    performance(pvalue = p_value(), sensitivity = as.numeric(input$sensitivity)) * 100
  })
  
  output$performance <- renderText({
    paste0("PERFORMANCE: ", round(perf(), 2), "%")
  })
  
  output$show_pvalue <- renderText({
    paste0("p-value: ", round(p_value(), 4))
  })
  
}


shinyApp(ui = ui, server = server)
