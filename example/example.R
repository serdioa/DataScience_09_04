

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

xval <- c(1:100)
yval <- rep(NA, 100)
for (i in 1:30) {
    yval[i] <- 75 - i
}
for (i in 31:70) {
    yval[i] <- 75 - 30 + (i - 30) * 2
}
for (i in 71:100) {
    yval[i] <- 75 - 30 + (70 - 30) * 2 - (i - 70)
}
for (i in 1:100) {
    yval[i] <- yval[i] + sin(pi * i / 7)* 10
}
yval <- yval + rnorm(100) * 4

plot(x = xval, y = yval, type = "l")

y10 <- sma(yval, 10)
y20 <- sma(yval, 20)

lines(x = xval, y = y20, type = "l", lwd = 2, col = "blue")
lines(x = xval, y = y10, type = "l", lwd = 2, col = "red")

