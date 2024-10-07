library(dplyr)
library(ggplot2)
library(plotly)
library(MASS)

maths_df <- read.csv(file = "./maths.csv", header = TRUE, row.names = NULL, encoding = "Latin1", sep = ",", dec = ".", quote = "\"", comment.char = "")

maths_df$class.f <- factor(maths_df$class)

n = dim(maths_df)[1]
k = length(levels(maths_df$class.f))

X <- matrix(0, n, k + 1)
X[, 1] <- 1
X[maths_df$class.f == 1, 2] <- 1
X[maths_df$class.f == 2, 3] <- 1
X[maths_df$class.f == 3, 4] <- 1

y = maths_df$maths.y

Xre <- X[, -1]
(b1 <- solve(t(Xre) %*% Xre, t(Xre) %*% y))

modelre <- lm(y ~ 0 + X[, 2] + X[, 3] + X[, 4])
summary(modelre)

XtXc <- matrix(0, 4, 4)
XtXc[2:4, 2:4] <- t(solve((t(X) %*% X)[2:4, 2:4]))
XtXc = t(XtXc)
(b2 <- XtXc %*% t(X) %*% y)
round(t(X) %*% X %*% b2 - t(X) %*% y, 6)

XtXc2 = ginv(t(X) %*% X)
b3 <- XtXc2 %*% t(X) %*% y
round(t(X) %*% X %*% b3 - t(X) %*% y, 6)

b2 + (diag(4) - XtXc %*% t(X) %*% X) %*% b3

tt1 = matrix(c(c(0, -1, 1, 0), c(0, 1, 0, -1), c(0, 0, -1, 1)), 3, 4, byrow=TRUE)
round(tt1 %*% XtXc %*% t(X) %*% X, 4)

tt1 %*% b2
tt1 %*% b3

matrix(c(0, -1, 1, 0), 1, 4) %*% b2
matrix(c(0, -1, 1, 0), 1, 4) %*% b3

e1 = y - X %*% b2
svar1 = sum(e1 ^ 2)
e2 = y - X %*% b3
svar2 = sum(e2 ^ 2)