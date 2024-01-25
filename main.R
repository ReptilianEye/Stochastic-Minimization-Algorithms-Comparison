library(ecr)
library(ggplot2)
library(smoof)

ackley_range <- c(-32.768, 32.768)
ackley_R2 <- makeAckleyFunction(2L)
ackley_R10 <- makeAckleyFunction(10L)
ackley_R20 <- makeAckleyFunction(20L)

rosenbrock_range <- c(-5, 10)
rosenbrock_R2 <- makeRosenbrockFunction(2L)
rosenbrock_R10 <- makeRosenbrockFunction(10L)
rosenbrock_R20 <- makeRosenbrockFunction(20L)

find_min_prs <- function(f, n, range) {
    R <- getParamLengths(getParamSet(f))
    min_v <- Inf
    for (i in 1:n) {
        v <- c(runif(R, range[1], range[2]))
        min_v <- min(min_v, f(v))
    }
    return(min_v)
}
prs_mean <- function(f, size, n, range) {
    y <- replicate(size, find_min_prs(f, n, range))
    return(mean(y))
}


f <- ackley_R2 # function to evaluate
size <- 50 # number of samples
n <- 1000 # number of iterations
range <- ackley_range # range of values for parameters
prs_mean(f, size, n, range)


find_min_ms <- function(f, n, range) {
    R <- getParamLengths(getParamSet(f))
    points <- matrix(0, nrow = n, ncol = R)
    for (i in 1:n) {
        points[i, ] <- runif(R, range[1], range[2])
    }
    min_v <- Inf
    lower <- replicate(R, range[1])
    upper <- replicate(R, range[2])
    calls <- 0
    for (i in 1:n) {
        result <- optim(points[i, ], f, lower = lower, upper = upper, method = "L-BFGS-B")
        min_v <- min(min_v, result$value)
        calls <- calls + result$counts[1]
        # min_v <- min(min_v, optim(points[i, ], f)$value, lower = lower, upper = upper, method = "L-BFGS-B")
    }
    return(c(min_v, calls))
}
ms_mean <- function(f, size, n, range) {
    result <- matrix(0, nrow = size, ncol = 2)
    for (i in 1:size) {
        result[i, ] <- find_min_ms(f, n, range)
    }
    # y <- replicate(size, matrix(find_min_ms(f, n, range)))
    y <- result[, 1]
    calls <- result[, 2]
    return(c(mean(y), mean(calls)))
}
f <- ackley_R2
n <- 100
size <- 50
range <- ackley_range
res <- ms_mean(f, size, n, range)
min_v <- res[1]
calls <- res[2]
prs_mean(ackley_R2, 50, calls, ackley_range)
