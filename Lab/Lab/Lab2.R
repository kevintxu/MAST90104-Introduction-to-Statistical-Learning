library(ggplot2)
library(plotly)

## 1
f1 = Vectorize (function(x) {
    if (x <= 0) {
        -x ^ 3
    }
    else if (x > 0 && x <= 1) {
        x^2
    }
    else {
        sqrt(x)
    }
})



x.values <- seq(-2, 2, by = 0.1)
# for each x calculate y
n <- length(x.values)
y.values <- rep(0, n)
for (i in 1:n) {
    x <- x.values[i]
    y = f1(x)
    y.values[i] <- y
}
# output
#plot(x.values, y.values, type = "l")
p = ggplot(mapping = aes(x = x.values, y = y.values)) +
    geom_line()
p

## 2
h2 = function(x, n) {
    y = 0
    for (i in 0:n) {
        y = y+x^i
    }
    y
}

## 3 
h2=Vectorize (h2)
h2(c(0.3, 6.6), c(55, 8))

## 5
rotate = function(v, rad) {
    matrix(c(cos(rad), sin(rad), - sin(rad), cos(rad)), nrow = 2, ncol = 2) %*% v
}
rotate(c(1, 1), pi)

## 6
geom_mean = function(x) {
    prod(x)^(1/length(x))
}
harmonic_mean = function(x) {
    1/(sum (1/x))
}

## 7
toggle_matrix = function(start_matrix, x) {
    start_matrix[seq(x, length(start_matrix), by = x)] = (start_matrix[seq(x, length(start_matrix), by = x)] + 1) %% 2
    return(start_matrix)
}

start_matrix = rep(0, each = 100)

for (i in 1:100) {
    start_matrix = toggle_matrix(start_matrix = start_matrix, x = i)
}
start_matrix