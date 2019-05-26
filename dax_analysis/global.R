# Load historical prices.
DAX <- readRDS("DAX.RDS")

# Prepare a list of symbols. Keep DAX first, sort the rest alphabetically.
symbols <- lapply(DAX, function(x) {x[["symbol"]]})
names(symbols) <- lapply(DAX, function(x) {x[["name"]]})
stockSymbols <- symbols[2:length(symbols)]
stockSymbols <- stockSymbols[order(names(stockSymbols))]
symbols <- c(symbols[1], stockSymbols)

# Available types of trend indicators.
trendTypes <- list("Simple moving average" = "SMA",
                   "Exponential moving average" = "EMA",
                   "Instantaneous Trendline" = "IT",
                   "Buttleworth 2-Pole" = "B2P",
                   "Buttleworth 3-Pole" = "B3P",
                   "Low-Pass" = "LP")

