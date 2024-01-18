# Poszukiwanie przypadkowe (Pure Random Search, PRS)
# Losujemy po kolei zadaną z góry liczbę punktów z rozkładem jednostajnym w zadanej dziedzinie. Jeżeli dziedzina jest kostką wielowymiarową (tu tak ma być), to można (i trzeba) losować kolejno współrzędne poszczególnych punktów według odpowiedniego jednowymiarowego rozkładu jednostajnego, np. jeśli dziedzina poszukiwania jest kostką trójwymiarową
# [0,1]×[−2,2]×[100,1000],
# to pierwszą współrzędną każdego punktu losujemy z rozkładu U(0,1)
# , drugą z rozkładu U(−2,2)
# , a trzecią z rozkładu U(100,1000)
# . Każdy wylosowany punkt porównujemy z aktualnie zapamiętanym minimum i jeśli wartość minimalizowanej funkcji w tym punkcie jest mniejsza, to ten punkt zapamiętujemy jako aktualny punkt minimalny. Wartość funkcji w ostatnim zapamiętanym punkcie stanowi wynik algorytmu.

# Path: project/main.R
# Compare this snippet from rozklady.r:
library(plot3D)

my_min <- function(a, b) {
    if (all(a < b)) {
        return(a)
    } else {
        return(b)
    }
}

prs <- function(n, ranges) {
    min_v <- Inf
    dis <- list()
    for (i in seq_along(ranges)) {
        dis[[i]] <- runif(n, ranges[[i]][[1]], ranges[[i]][[2]])
    }
    for (i in 1:n) {
        v <- c()
        for (j in seq_along(ranges)) {
            v <- c(v, dis[[j]][i])
        }
        min_v <- my_min(min_v, v)
    }
    return(min_v)
    # for (i in 1:n) {
    #     x <- runif(1, 0, 1)
    #     y <- runif(1, -2, 2)
    #     z <- runif(1, 100, 1000)
    #     v <- c(x, y, z)
    #     min_v <- my_min(min_v, v)
    # }
    return(min_v)
}
ranges <- list(list(0, 1), list(-2, 2), list(100, 1000))
# y <- prs(1000, ranges)
# y
y <- replicate(10, list(prs(1000, list(list(0, 1), list(-2, 2), list(100, 1000)))))

mean_y <- rep(0, length(ranges))
for (vector in y) {
    for (i in seq_along(vector)) {
        mean_y[i] <- mean_y[i] + vector[i]
    }
}
for (i in seq_along(mean_y)) {
    mean_y[i] <- mean_y[i] / length(y)
}
y
mean_y
