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

    getReactivePrice <- reactive({
        DAX[[input$symbol]]$ts
    })

    getReactiveFastTrend <- reactive({
        ts <- getReactivePrice()
        buildTrend(ts$adjusted, input$days[1], input$fastIndicatorType)
    })

    getReactiveSlowTrend <- reactive({
        ts <- getReactivePrice()
        buildTrend(ts$adjusted, input$days[2], input$slowIndicatorType)
    })

    getReactiveTrades <- reactive({
        fastTrend <- getReactiveFastTrend()
        slowTrend <- getReactiveSlowTrend()
        trades <- findTrades(fastTrend, slowTrend)
    })

    getReactiveCapital <- reactive({
        ts <- getReactivePrice()
        trades <- getReactiveTrades()

        buildCapital(ts$ts, ts$adjusted, trades)
    })

    getReactiveCapitalFitted <- reactive({
        ts <- getReactivePrice()
        capital <- getReactiveCapital()

        capitalFitted <- lm(capital ~ ts$ts) %>% fitted.values()
    })

    getReactiveTradeDescription <- reactive({
        ts <- getReactivePrice()
        trades <- getReactiveTrades()

        tradeDescription <- buildTradeDescription(ts$ts, ts$adjusted, trades)
    })

    output$distPlot <- renderPlotly({
        ts <- getReactivePrice()
        # ts <- DAX[[input$symbol]]$ts

        pricePlot <- plot_ly(x = ts$ts)
        pricePlot <- pricePlot %>%
            add_trace(y = ts$adjusted, name = 'Price', type = 'scatter', mode = 'lines',
                      line = list(
                          color = 'rgb(0, 0, 0)',
                          width = 1.5
                      ))

        # fastTrend <- buildTrend(ts$adjusted, input$days[1], input$fastIndicatorType)
        # slowTrend <- buildTrend(ts$adjusted, input$days[2], input$slowIndicatorType)
        fastTrend <- getReactiveFastTrend()
        slowTrend <- getReactiveSlowTrend()

        pricePlot <- pricePlot %>%
             add_trace(y = fastTrend, name = 'Fast', type = 'scatter', mode = 'lines',
                       line = list(
                           color = 'rgb(255, 140, 0)',
                           width = 2
                       ))
         pricePlot <- pricePlot %>%
             add_trace(y = slowTrend, name = 'Slow', type = 'scatter', mode = 'lines',
                       line = list(
                           color = 'rgb(0, 64, 255)',
                           width = 2
                       ))

         trades <- getReactiveTrades()

         buyTrades <- rep(NA, length(fastTrend))
         buyTrades[trades$buy] <- ts$adjusted[trades$buy]

         sellTrades <- rep(NA, length(fastTrend))
         sellTrades[trades$sell] <- ts$adjusted[trades$sell]

         pricePlot <- pricePlot %>%
             add_trace(y = buyTrades, name = 'Buy', type = 'scatter', mode = 'markers',
                       marker = list(
                           color = 'rgb(0, 192, 0)',
                           size = 12
                       )) %>%
             add_trace(y = sellTrades, name = 'Sell', type = 'scatter', mode = 'markers',
                       marker = list(
                           color = 'rgb(192, 0, 0)',
                           size = 12
                       )) %>%
             layout(yaxis = list(
                 title = "Price"
             ))

        # Calculate development of capital.
        # Multiply by 100 to get as %.
        capital <- getReactiveCapital() * 100
        capitalFitted <- getReactiveCapitalFitted() * 100

        capitalPlot <- plot_ly(x = ts$ts) %>%
            add_trace(y = capital, name = "Capital, %", type = "scatter", mode = "lines",
                      line = list(
                          color = 'rgb(0, 0, 0)',
                          width = 1.5
                      )) %>%
        add_trace(y = capitalFitted, name = "Capital LM", type = "scatter", mode = "lines",
                  line = list(
                      color = 'rgb(0, 0, 192)',
                      width = 2
                  )) %>%
            layout(yaxis = list(
                title = "Capital, %"
            ))

        # Combine plots
        combinedPlot <- subplot(pricePlot, capitalPlot, nrows = 2, shareX = TRUE,
                                titleY = TRUE,
                                heights = c(0.65, 0.35)) %>%
            layout(xaxis = list(
                title = "Date"
            ))

        return (combinedPlot)
    })


    output$statistics <- renderTable({
        ts <- getReactivePrice()
        trades <- getReactiveTrades()
        tradeDescription <- getReactiveTradeDescription()
        capital <- getReactiveCapital()

        # Total number of trades
        numberOfTrades <- nrow(tradeDescription)

        # Percentage of profitable trades
        numberOfProfitableTrades <- nrow(tradeDescription[tradeDescription$pl.abs > 0,])
        if (numberOfTrades > 0) {
            pctOfProfitableTrades <- numberOfProfitableTrades / numberOfTrades
        } else {
            pctOfProfitableTrades <- 0
        }

        # Average keep time for a trade.
        averageKeepTime <- mean(apply(tradeDescription, 1, function(x) {as.numeric(as.Date(x[4]) - as.Date(x[2]))}))

        # Annualized rate of return.
        totalRateOfReturn <- capital[length(capital)] - 1.0
        totalDurationDays <- as.numeric(ts$ts[nrow(ts)] - ts$ts[1])
        annualizedRateOfReturn <- totalRateOfReturn * 365 / totalDurationDays

        data.frame(c("Number of trades",
                     "Average time period between trades, days",
                     "Percentage of profitable trades",
                     "Annualized rate of return, %"),
                   c(sprintf("%.0f", numberOfTrades),
                     sprintf("%.0f", averageKeepTime),
                     sprintf("%.2f%%", pctOfProfitableTrades * 100),
                     sprintf("%.2f%%", annualizedRateOfReturn * 100)))
    }, colnames = FALSE)

    output$trades <- renderDataTable({
        # ts <- DAX[[input$symbol]]$ts

        #fastTrend <- buildTrend(ts$adjusted, input$days[1], input$fastIndicatorType)
        #slowTrend <- buildTrend(ts$adjusted, input$days[2], input$slowIndicatorType)

        # trades <- findTrades(fastTrend, slowTrend)
        # tradeDescription <- buildTradeDescription(ts$ts, ts$adjusted, trades)

        tradeDescription <- getReactiveTradeDescription()

        # Transform to %
        tradeDescription$pl.pct <- round(tradeDescription$pl.pct * 100, digits = 4)
        tradeDescription$pl.annualized <- round(tradeDescription$pl.annualized * 100, digits = 4)

        names(tradeDescription) <- c("Direction", "Entry Date", "Entry Price",
                                     "Exit Date", "Exit Price", "Trade P/L",
                                     "Rate of Return, %", "Annualized Rate of Return, %")

        return (tradeDescription)
    })
})

buildTrend <- function(x, n, type) {
    if (type == "SMA") {
        return (sma(x, n))
    } else if (type == "EMA") {
        return (ema(x, n))
    } else if (type == "IT") {
        return (insttrend(x, n))
    } else if (type == "B2P") {
        return (buttl2p(x, n))
    } else if (type == "B3P") {
        return (buttl3p(x, n))
    } else if (type == "LP") {
        return (decycler(x, n))
    }
}

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

buttl2p <- function(x, n) {
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

buttl3p <- function(x, n) {
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

decycler <- function(x, n) {
    if (length(x) <= 0) {
        return (x)
    }

    arg = 2.0 * pi / n;
    c = cos(arg);
    s = sin(arg);
    alpha = (c + s - 1) / c;

    c1 = alpha / 2.0;
    c2 = (1.0 - alpha);

    y <- x
    for (i in 1:length(x)) {
        if (i == 1) {
            y[i] = x[i]
        } else {
            y[i] <- c1 * (x[i] + x[i - 1]) + c2 * y[i - 1]
        }
    }

    return (y)
}


findTrades <- function(fast, slow) {
    lng <- length(fast)
    trades <- data.frame(fast = fast, slow = slow)
    trades$buy <- FALSE
    trades$sell <- FALSE

    # TRUE - long, FALSE - short
    side <- NA
    for (i in 1:lng) {
        if (is.na(side)) {
            # We do not have any position yet. Open position based on fast/slow:
            # if fast is above slow - open long position,
            # if fast is below slow - open short position.
            if (fast[i] > slow[i]) {
                side <- TRUE
                trades$buy[i] <- TRUE
            } else {
                side <- FALSE
                trades$sell[i] <- TRUE
            }
        } else if (side) {
            # Currently holding a long position.
            # If fast goes below slow, sell.
            if (fast[i] < slow[i]) {
                side = FALSE
                trades$sell[i] <- TRUE
            }
        } else {
            # Currently holding a short position.
            # If fast goes above slow, buy.
            if (fast[i] > slow[i]) {
                side = TRUE
                trades$buy[i] <- TRUE
            }
        }
    }

    return (trades)
}


buildTradeDescription <- function(ts, price, trades) {
    pos.direction <- c()
    entry.date <- ts[0:0]
    entry.price <- c()
    exit.date <- ts[0:0]
    exit.price <- c()
    pl.abs <- c()
    pl.pct <- c()
    pl.annualized <- c()

    curr.pos.direction <- NA
    curr.entry.date <- NA
    curr.entry.price <- NA

    for (i in 1:length(ts)) {
        if (trades$buy[i] | trades$sell[i]) {
            if (!is.na(curr.pos.direction)) {
                # Has an open position. Create a statistical record.

                pos.direction <- c(pos.direction, curr.pos.direction)
                entry.date <- c(entry.date, curr.entry.date)
                entry.price <- c(entry.price, curr.entry.price)
                exit.date <- c(exit.date, ts[i])
                exit.price <- c(exit.price, price[i])

                curr.pl.abs <- price[i] - curr.entry.price
                if (curr.pos.direction == "SHORT") {
                    curr.pl.abs <- -curr.pl.abs
                }
                pl.abs <- c(pl.abs, curr.pl.abs)

                curr.pl.pct <- curr.pl.abs / curr.entry.price
                pl.pct <- c(pl.pct, curr.pl.pct)

                curr.pos.duration <- as.integer(ts[i] - curr.entry.date)
                pl.annualized <- c(pl.annualized, curr.pl.pct * 365 / curr.pos.duration)
            }

            if (trades$buy[i]) {
                curr.pos.direction <- "LONG"
            } else {
                curr.pos.direction <- "SHORT"
            }
            curr.entry.date <- ts[i]
            curr.entry.price <- price[i]
        }
    }

    data.frame("pos.direction" = pos.direction,
               "entry.date" = entry.date,
               "entry.price" = entry.price,
               "exit.date" = exit.date,
               "exit.price" = exit.price,
               "pl.abs" = pl.abs,
               "pl.pct" = pl.pct,
               "pl.annualized" = pl.annualized)
}



buildCapital <- function(ts, price, trades) {
    # Calculating capital changes in %. To get results in %, use initial capital
    # of 1.0.
    initialCapital <- 1.0

    capital <- vector(mode = "numeric", length = length(ts))

    curr.pos.direction <- NA
    curr.pos.size <- NA
    curr.entry.price <- NA
    curr.entry.volume <- NA

    for (i in 1:length(ts)) {
        # Calculate current capital
        if (is.na(curr.pos.direction)) {
            # No trades done yet
            capital[i] <- initialCapital
        } else {
            # Updating capital
            curr.price.diff <- price[i] - curr.entry.price
            if (curr.pos.direction == "SHORT") {
                curr.price.diff <- -curr.price.diff
            }
            curr.volume.diff <- curr.pos.size * curr.price.diff
            capital[i] <- curr.entry.volume + curr.volume.diff
        }

        if (trades$buy[i] | trades$sell[i]) {
            # Executing trade

            if (is.na(curr.pos.direction)) {
                # Very first trade.
                curr.pos.size <- initialCapital / price[i]
            } else {
                # Calculate current volume
                curr.pos.size <- capital[i] / price[i]
            }

            if (trades$buy[i]) {
                curr.pos.direction <- "LONG"
            } else {
                curr.pos.direction <- "SHORT"
            }
            curr.entry.price <- price[i]
            curr.entry.volume <- curr.pos.size * curr.entry.price
        }
    }

    return (capital)
}

