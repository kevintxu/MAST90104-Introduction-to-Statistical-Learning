library(dplyr)
library(MASS)
library(ggplot2)

## 3
det.kev = function(m) {
    if (!is.matrix(m)) stop("m must be a matrix")

    dimension = dim(m)
    retVal = numeric(0)
    if (dimension[1] != dimension[2]) {
        stop("m must be a square matrix")
    } else if (dimension[1] == 1) {
        retVal = m[1,1]
    } else if (identical(dimension, c(2L, 2L))) {
        retVal = m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1]
    } else {
        retVal = 0
        not_zeros = data.frame(not_zero = integer(0), number = integer(0), row_col = character(0))
        for (i in 1:dimension[1]) {
            new_row = list(not_zero = length(m[i,][m[i,] != 0]), number = i, row_col = "r")
            not_zeros = rbind(not_zeros, new_row, stringsAsFactors = FALSE)
        }
        for (i in 1:dimension[2]) {
            new_row = list(not_zero = length(m[, i][m[, i] != 0]), number = i, row_col = "c")
            not_zeros = rbind(not_zeros, new_row, stringsAsFactors = FALSE)
        }
        not_zeros = not_zeros %>% arrange(not_zero)
        best_path = not_zeros[1,]
        best_row_col = c(numeric(0))

        if (best_path$row_col == "r") {
            best_row_col = m[best_path$number,]
        } else {
            best_row_col = m[, best_path$number]
        }

        index = integer(0)
        if (best_path$number %% 2 == 1) {
            index = seq(2, length(best_row_col), by = 2)
        } else {
            index = seq(1, length(best_row_col), by = 2)
        }

        best_row_col[index] = best_row_col[index] * -1

        if (best_path$row_col == "r") {
            for (i in 1:length(best_row_col)) {
                if (best_row_col[i] != 0) {
                    retVal = retVal + (best_row_col[i] * det.kev(m[-best_path$number, -i]))
                }
            }
        } else {
            for (i in 1:length(best_row_col)) {
                if (best_row_col[i] != 0) {
                    retVal = retVal + (best_row_col[i] * det.kev(m[-i, -best_path$number]))
                }
            }
        }
    }
    return (retVal)
}
#det.kev(a)
 #q4_B_solve = matrix(c(
   #c(0.8,-0.4,0,0,0,0,0,0,0),
   #c(0,0,0,-0.4,0.2,0,0,0,0),
   #c(0,0,0,0,0,0,0,0,1),
   #c(-0.4,0.2,0,0.8,-0.4,0,0,0,0),
   #c(0,0,1,0,0,0,0.8,0.4,0),
   #c(0,0,0,0,0,1,-0.4,0.2,0)
   #), 9, 6
 #)

## 2 
x = c(8, 12, 14, 16, 16, 20)
y = c(8, 15, 16, 20, 25, 40)
m2c = matrix(c(x, y), nrow = length(x), ncol = 2)
dimnames(m2c) = list(NULL, c("years_of_education", "income_in_Ks"))

### a
p = ggplot(mapping = aes(x = m2c[, "years_of_education"], y = m2c[, "income_in_Ks"])) +
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
model = lm(y ~ x)
rstandard(model)
H = X %*% ginv(t(X) %*% X) %*% t(X)
e = (diag(1, nrow = dim(H)[1], ncol = dim(H)[2]) - H) %*% y
z = c(e / sqrt((svar) * (1 - c(diag(H)))))

H[1, 1]
influence(model)

cooks.distance(model)
cd = 1 / (1 + 1) * z ^ 2 * (diag(H) / (1 - diag(H)))

## 1
### a)
s = sqrt(svar)
n = 6
p = 2
c = c(diag(ginv(t(X) %*% X)))
b_interval = qt(1 - .05 / 2, df = n - p) * s * sqrt(c)

### b

xs = matrix(c(1, 18), 2, 1)

t(xs) %*% b

ys_ci = qt(1 - .05 / 2, df = n - p) * s * sqrt(t(xs) %*% ginv(t(X) %*% X) %*% xs)

### c
ys_pi = qt(1 - .05 / 2, df = n - p) * s * sqrt(1 + t(xs) %*% ginv(t(X) %*% X) %*% xs)
