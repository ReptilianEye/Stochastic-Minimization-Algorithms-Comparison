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
prs_rep <- function(f, size, n, range) {
    y <- replicate(size, find_min_prs(f, n, range))
    return(y)
}

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
    }
    return(c(min_v, calls))
}
ms_rep <- function(f, size, n, range) {
    result <- matrix(0, nrow = size, ncol = 2)
    for (i in 1:size) {
        result[i, ] <- find_min_ms(f, n, range)
    }
    y <- result[, 1]
    calls <- result[, 2]
    return(list(y, mean(calls)))
}
test_runner <- function(f, size, starting_points_n, range) {
    res <- ms_rep(f, size, starting_points_n, range)
    ms <- res[[1]] # minima found by MS
    calls <- res[[2]] # mean of calls
    prs <- prs_rep(f, size, calls, range) # mean of minima with same number of calls as MS
    return(c(ms = list(ms), prs = list(prs), ms_min = mean(ms), prs_min = mean(prs)))
}

f <- ackley_R20
n <- 100 # number of starting points
size <- 50 # number of repetitions
range <- ackley_range # range of starting points
res <- test_runner(f, size, n, range)
ms <- res$ms
prs <- res$prs
t.test(ms, prs, mu = 0)
