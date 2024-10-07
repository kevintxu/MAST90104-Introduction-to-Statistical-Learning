library(MASS)
library(ggplot2)
library(plotly)

## 3
f3 = function(v) {
    sqrt(sum(v^2))
}

## 4


## 5
sample(1:6, size = 1)

### a
game5a = function() {
    results = sample(1:6, size = 4, replace = TRUE)
    
    results
}

### b
sixes = function(n) {
    results = sample(1:6, size = n, replace = TRUE)
    ##print(results)

    if (length(results[results == 6]) > 0) { return(TRUE) }
    else {return (FALSE) }
}


## 2 
x = c(8, 12, 14, 16, 16, 20)
y = c(8, 15, 16, 20, 25, 40)
m2c = matrix (c(x,y), nrow = length(x), ncol = 2)
dimnames(m2c) = list(NULL, c("years_of_education", "income_in_Ks"))

### a
p = ggplot(mapping=aes(x = m2c[, "years_of_education"], y = m2c[,"income_in_Ks"])) +
    geom_point()
p

### d
y = c(8, 15, 16, 20, 25, 40)
X = matrix(c(rep(1, 6), x), nrow = 6, ncol = 2)

qr(X)$rank

b = ginv(t(X) %*% X) %*% t(X) %*% y

p = p + geom_abline(slope = b[2], intercept = b[1])
p

### e
b_1 = (mean(x * y) - mean(x) * mean(y)) / (mean(x ^ 2) - (mean(x)) ^ 2)
b_0 = mean(y) - b_1 * mean(x)

### f
svar = c(t(y - X %*% b) %*% (y - X %*% b) / (6 - (1 + 1)))

### g
c(1, 18) %*% b

### h
model = lm(y~x)
rstandard(model)
H = X %*% ginv(t(X) %*% X) %*% t(X)
e = (diag(1, nrow = dim(H)[1], ncol = dim(H)[2]) - H) %*% y
z = c(e / sqrt((svar) * (1 - c(diag(H)))))

H[1, 1]
influence(model)

cooks.distance(model)
cd = 1/(1+1) * z^2 * (diag(H)/(1-diag(H)))