
## 1
x = 123
a = 1.1
b = 1.2
### a
z = x ^ (a ^ b)
### b
z = (x ^ a) ^ b
### c
z = matrix(c(3, 2, 6, 1), nrow = 1, ncol = 4) %*% matrix(c(x ^ 3, x ^ 2, x, 1), nrow = 4, ncol = 1)
### d
z = floor(x / 10) %% 10
### e
z = z + 1

## 2
### a
c(seq(1,8),seq(7,1))
### b
c(rep(1, 1), rep(2, 2), rep(3, 3), rep(4, 4), rep(5, 5))
### c
1 - diag(3)
### d
a1 = matrix(c(0, 0, 7, 2, 5, 0, 3, 0, 0), nrow = 3, ncol = 3)
c(1, 2, 3)

## 3
vec = c(1, sqrt(3))
polarVec = c(2, atan(vec[2] / vec[1]))

## 4
a = 1:100
a2 = a %% 2
a3 = a %% 3
a7 = a %% 7

a3[a3 > 0] = 1
a7[a7 > 0] = 1

aa = (a * a2 * a3 * a7)
aa = aa[aa != 0]

aa = a[(a %% 2 != 0) & (a %% 3 != 0) & (a %% 7 != 0) ]

## 5
queue = c("Steve", "Russell", "Alison", "Liam")
### a
queue = c(queue, "Barry")
### b
queue = queue[-1]
### c
queue = c("Pam", queue)
### d
queue = queue[queue != "Barry"]
### e
queue = queue[queue != "Alison"]

which (queue == "Russell")
queue[queue == "Russell"]

## 6
rm(list = ls())
x <- 1
x[3] <- 3
y <- c()
y[2] <- 2
y[3] <- y[1]
y[2] <- y[4]
z[1] <- 0

## 7
diag(10) * 5

i=diag(10)
i[i!=0]=5