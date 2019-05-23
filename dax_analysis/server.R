#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$distPlot <- renderPlotly({

    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')

      ts <- DAX[[input$symbol]]$ts
      # plot(x = ts$ts, y = ts$close, type = "l")
      # plot_ly(x = ts$ts, y = ts$close, type = 'scatter', mode = 'lines')
      plot <- plot_ly(x = ts$ts)
      if (input$showPrice) {
        plot <- plot %>%
              add_trace(y = ts$close, name = 'Price', type = 'scatter', mode = 'lines')
      }
      if (input$showTrend) {
          if (input$trendType == "SMA") {
              plot <- plot %>%
                  add_trace(y = sma(ts$close, input$days), name = 'SMA', type = 'scatter', mode = 'lines')
          } else if (input$trendType == "EMA") {
              plot <- plot %>%
                  add_trace(y = ema(ts$close, input$days), name = 'EMA', type = 'scatter', mode = 'lines')
          } else if (input$trendType == "IT") {
              plot <- plot %>%
                  add_trace(y = insttrend(ts$close, input$days), name = 'Inst. Trendline', type = 'scatter', mode = 'lines')
          } else if (input$trendType == "B2P") {
              plot <- plot %>%
                  add_trace(y = buttl2p(ts$close, input$days), name = 'Buttleworth 2-Pole', type = 'scatter', mode = 'lines')
          } else if (input$trendType == "B3P") {
              plot <- plot %>%
                  add_trace(y = buttl3p(ts$close, input$days), name = 'Buttleworth 3-Pole', type = 'scatter', mode = 'lines')
          }
      }
      return (plot)
  })

  output$selectedShowPrice <- renderText({
      input$showPrice
  })

  output$selectedTrendType <- renderText({
      input$trendType
  })

  output$selectedShowTrend <- renderText({
      input$showTrend
  })
})

sma <- function(x, n) {
    if (length(x) <= 0) {
        return (x)
    }

    y <- x
    sum = 0
    mean = 0;
    for (i in 1:length(x)) {
        sum = sum + x[i]
        if (i > n) {
            sum = sum - x[i - n]
        }

        if (i < n) {
            mean = sum / i
        } else {
            mean = sum / n
        }

        y[i] = mean
    }

    return (y)
}

ema <- function(x, n) {
    if (length(x) <= 0) {
        return (x)
    }

    alpha <- 2 / (1 + n)
    beta = 1 - alpha
    mean <- NA
    y <- x
    for (i in 1:length(x)) {
        if (is.na(mean)) {
            mean <- x[i]
        } else {
            mean <- mean * beta + x[i] * alpha
        }
        y[i] <- mean
    }

    return (y)
}

insttrend <- function(x, n) {
    if (length(x) <= 0) {
        return (x)
    }

    alpha <- 2 / (1 + n)
    i0 <- alpha - alpha*alpha/4.0
    i1 <- 0.5 * alpha * alpha
    i2 <- -(alpha - 0.75 * alpha * alpha)
    t1 <- 2.0 * (1 - alpha)
    t2 <- -(1 - alpha) * (1 - alpha)

    y <- x
    for (i in 1:length(x)) {
        v0 = x[i]
        if (i > 1) {
            v1 = x[i - 1]
        } else {
            v1 = x[i]
        }
        if (i > 2) {
            v2 = x[i - 2]
        } else if (i > 1) {
            v2 = x[i - 1]
        } else {
            v2 = x[i]
        }

        if (i < 7) {
            y[i] = (v0 + 2.0 * v1 + v2) / 4.0;
        } else {
            y[i] = i0 * v0 + i1 * v1 + i2 * v2 + t1 * y[i - 1] + t2 * y[i - 2];
        }
    }

    return (y)
}

buttl2p = function(x, n) {
    if (length(x) <= 0) {
        return (x)
    }

    a <- exp(-1.414 * pi / n);
    b <- 2.0 * a * cos(1.414 * pi / n);
    c1 <- (1.0 - b + a * a) / 4.0;
    c2 <- b;
    c3 <- -a * a;

    y <- x
    for (i in 1:length(x)) {
        if (i < 4) {
            y[i] <- x[i];
        } else {
            y[i] <- c1 * (x[i] + 2 * x[i - 1] + x[i - 2]) +
             c2 * y[i - 1] + c3 * y[i - 2];
        }
    }

    return (y)
}

buttl3p = function(x, n) {
    if (length(x) <= 0) {
        return (x)
    }

    a <- exp(-pi / n);
    b <- 2.0 * a * cos(1.738 * pi / n);
    c <- a * a
    c1 <- (1 - b + c) * (1 - c) /8;
    c2 <- b + c;
    c3 <- -(c + b * c);
    c4 <- c * c

    y <- x
    for (i in 1:length(x)) {
        if (i < 4) {
            y[i] <- x[i];
        } else {
            y[i] <- c1 * (x[i] + 3 * x[i - 1] + 3 * x[i - 2] + x[i - 3]) +
                c2 * y[i - 1] + c3 * y[i - 2] + c4 * y[i - 3];
        }
    }

    return (y)
}
