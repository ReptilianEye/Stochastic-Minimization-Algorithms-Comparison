my_min <- function(a, b) {
    if (all(a < b)) {
        return(a)
    } else {
        return(b)
    }
}

prs_unit <- function(n, ranges) {
    dis <- list()
    for (i in seq_along(ranges)) {
        dis[[i]] <- runif(n, ranges[[i]][[1]], ranges[[i]][[2]])
    }
    min_v <- Inf
    for (i in 1:n) {
        v <- c()
        for (j in seq_along(ranges)) {
            v <- c(v, dis[[j]][i])
        }
        min_v <- my_min(min_v, v)
    }
    return(min_v)
}
prs_mean <- function(size, n, ranges) {
    y <- replicate(size, list(prs_unit(n, ranges)))
    mean_y <- rep(0, length(ranges))
    for (vector in y) {
        for (i in seq_along(vector)) {
            mean_y[i] <- mean_y[i] + vector[i]
        }
    }
    print(y)
    for (i in seq_along(mean_y)) mean_y[i] <- mean_y[i] / length(y)
    return(mean_y)
}
size <- 2
n <- 100
ranges <- list(list(0, 1), list(-2, 2), list(100, 1000))
prs_mean(size, n, ranges)
