library(MASS)
library(Matrix)

## 1

### a)
filters <- read.csv(file = "./filters.csv", header = TRUE, row.names = NULL, encoding = "Latin1", sep = ",", dec = ".", quote = "\"", comment.char = "")

### b)
X = matrix(
        c(c(rep(1, 30)),
        c(rep(1, 6), rep(0, 24)),
        c(rep(0, 6), rep(1, 6), rep(0, 18)),
        c(rep(0, 12), rep(1, 6), rep(0, 12)),
        c(rep(0, 18), rep(1, 6), rep(0, 6)),
        c(rep(0, 24), rep(1, 6))), nrow = 30, ncol = 6
    )

y = filters$life

### c)
XTX = t(X) %*% X
M = XTX[2:6, 2:6]
Minv_T = t(solve(M))
XTXc = matrix(0,nrow=6,ncol=6)
XTXc[2:6, 2:6] = Minv_T
XTXc = t(XTXc)

n = dim(X)[1]
p = dim(X)[2]

### d)
XTX_ginv = ginv(XTX)

### e)
round(X %*% XTXc %*% t(X), 6) == round(X %*% XTX_ginv %*% t(X), 6)

### f)
b1 = XTXc %*% t(X) %*% y
b2 = XTX_ginv %*% t(X) %*% y

### g)
round(b1 + (diag(p) - XTXc %*% t(X) %*% X) %*% b2, 6) == round(b2, 6)

### i)
t1 = c(1, -1, 0, 0, 0, 1)
round(t(t1) %*% XTXc %*% t(X) %*% X, 6)
#### not estimable

### j)
t2 = c(0, 1, 0, -.5, -.5, 0)
round(t(t2) %*% XTXc %*% t(X) %*% X, 6)
#### estimable

### k)
t3 = c(0, 0, 0, 0, 1, -1)
round(t(t3) %*% XTXc %*% t(X) %*% X, 6)

t(t3) %*% b1
t(t3) %*% b2

### l)
t4 = c(2, 1, 0, 0, 0, 0)
round(t(t4) %*% XTXc %*% t(X) %*% X, 6)

t(t4) %*% b1
t(t4) %*% b2

### m)
#### i
t5 = c(1, 1, 0, 0, 0, 0)
round(t(t5) %*% XTXc %*% t(X) %*% X, 6)
t(t5) %*% b1

#### ii
t6 = c(0, 0, 1, -1, 0, 0)
round(t(t6) %*% XTXc %*% t(X) %*% X, 6)
t(t6) %*% b1

#### iii
t7 = c(0, -1/5, -1/5, -1/5, 4/5, -1/5)
round(t(t7) %*% XTXc %*% t(X) %*% X, 6)
t(t7) %*% b1

#### iv
t8 = c(5, 1, 1, 1, 1, 1)
round(t(t8) %*% XTXc %*% t(X) %*% X, 6)
t(t8) %*% b1

### n)
filters$type = factor(filters$type)
model = lm(life ~ type, data = filters)