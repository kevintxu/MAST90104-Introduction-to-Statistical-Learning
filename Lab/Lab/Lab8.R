library(MASS)
library(Matrix)
library(car)
library(tidyverse)
library(gmodels)

## 1
filters <- read.csv(file = "./filters.csv", header = TRUE, row.names = NULL, encoding = "Latin1", sep = ",", dec = ".", quote = "\"", comment.char = "")
filters$type = factor(filters$type)
X = matrix(
        c(c(rep(1, 30)),
        c(rep(1, 6), rep(0, 24)),
        c(rep(0, 6), rep(1, 6), rep(0, 18)),
        c(rep(0, 12), rep(1, 6), rep(0, 12)),
        c(rep(0, 18), rep(1, 6), rep(0, 6)),
        c(rep(0, 24), rep(1, 6))), nrow = 30, ncol = 6
    )

y = filters$life


XTX = t(X) %*% X
M = XTX[2:6, 2:6]
Minv_T = t(solve(M))
XTXc = matrix(0, nrow = 6, ncol = 6)
XTXc[2:6, 2:6] = Minv_T
XTXc = t(XTXc)

n = dim(X)[1]
p = dim(X)[2]
r = rankMatrix(X)[1]


XTX_ginv = ginv(XTX)


round(X %*% XTXc %*% t(X), 6) == round(X %*% XTX_ginv %*% t(X), 6)


b1 = XTXc %*% t(X) %*% y
b2 = XTX_ginv %*% t(X) %*% y


### a)
H = X %*% XTXc %*% t(X)

SSRes = c(t(y - X %*% b1) %*% (y - X %*% b1))
SSRes = c(t(y) %*% (diag(n) - H) %*% y)

model = lm(life ~ 1 + type, data = filters)
model_h0 = lm(life ~ 1, data = filters)

svar = SSRes / (n - r)

### b)
t1 = c(0,0,0,1,-1,0)
round(t(t1) %*% XTXc %*% t(X) %*% X, 6)
t_a = qt(0.975, df = (n - r))

c(t(t1) %*% b1) + c(-1, 1) * t_a * sqrt(svar) * sqrt(c(t(t1) %*% XTXc %*% t1))

### c)
C1 = matrix(
    c(0, 1, -1, 0, 0, 0,
        0, 1, 0, -1, 0, 0,
        0, 1, 0, 0, -1, 0,
        0, 1, 0, 0, 0, -1),
        nrow = 4, ncol = p, byrow = T
)

round(C1 %*% XTXc %*% t(X) %*% X, 6)

### d)
r2 = rankMatrix(C1)[1]

Fstat = (t(C1 %*% b1) %*% ginv(C1 %*% XTXc %*% t(C1)) %*% (C1 %*% b1) / r2) / (SSRes / (n - r))

1 - pf(Fstat, df1 = r2, df2 = (n - r))

### e)
C2 = matrix(
    c(  0, 1, 0, 0, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 1, 0,
        0, 0, 0, 0, 1),
        nrow = 4, ncol = 5, byrow = T
)
linearHypothesis(model, C2, test="F")

## 2
XtX = matrix(c(100 + 85 + 90, 100, 85, 90,
    100, 100, 0, 0,
    85, 0, 85, 0,
    90, 0, 0, 90),
    nrow = 4, ncol = 4, byrow = T)
Xt_y = c(9 * 100 + 6.2 * 85 + 10.1 * 90, 9 * 100, 6.2 * 85, 10.1 * 90)
n = 100 + 85 + 90
r = rankMatrix(XtX)[1]

XtXc = matrix(0, nrow = 4, ncol = 4)
M = XtX[2:4, 2:4]
XtXc[2:4, 2:4] = t(solve(M))
XtXc = t(XtXc)
b = XtXc %*% Xt_y

### a)
C = matrix(c(0, 1, -1, 0,
    0, 1, 0, -1),
    nrow = 2, ncol = 4, byrow = T)
round(C %*% XtXc %*% XtX, 6) == C
r2 = rankMatrix(C)[1]

Fstat = (t(C %*% b) %*% ginv(C %*% XtXc %*% t(C)) %*% (C %*% b) / r2) / (110.15)
1 - pf(Fstat, df1 = r2, df2 = (n - r))

### b)
C = matrix(c(0, 1, 0, -1),
    nrow = 1, ncol = 4, byrow = T)
round(C %*% XtXc %*% XtX, 6) == C
r2 = rankMatrix(C)[1]

Fstat = (t(C %*% b) %*% ginv(C %*% XtXc %*% t(C)) %*% (C %*% b) / r2) / (110.15)
1 - pf(Fstat, df1 = r2, df2 = (n - r))


## 3

### a
cows_df = as.data.frame(matrix(c(
    1, 1, 18.8,
    1, 1, 21.2,
    2, 1, 22.3,
    1, 2, 16.7,
    2, 2, 15.9,
    2, 2, 19.2,
    1, 3, 19.8,
    1, 3, 23.9,
    2, 3, 21.8
    ), nrow = 9, ncol = 3, byrow = TRUE))

colnames(cows_df) = c("breed", "diet", "yield")

cows_df$breed = factor(cows_df$breed)
cows_df$diet = factor(cows_df$diet)

cows_df[c(paste("breed", levels(cows_df$breed), sep = "."), paste("diet", levels(cows_df$diet), sep = "."))] = 0

for (col_n in colnames(cows_df[, -1:-3])) {
    row_n = paste("breed", cows_df$breed, sep = ".") == col_n
    cows_df[row_n, col_n] = 1
}
for (col_n in colnames(cows_df[, -1:-3])) {
    row_n = paste("diet", cows_df$diet, sep = ".") == col_n
    cows_df[row_n, col_n] = 1
}

### a
q3a_X = as.matrix(cows_df %>% mutate(intercept = 1) %>% select(intercept, starts_with("breed."), starts_with("diet.")))

### b
col_n = merge(paste("breed", levels(cows_df$breed), sep = "."), paste("diet", levels(cows_df$diet), sep = "."), all = TRUE)
col_n = paste(col_n$x, col_n$y, sep = ":")
cows_df[col_n] = 0
for (col_n in colnames(cows_df[, -1:-3])) {
    row_n = paste(paste("breed", cows_df$breed, sep = "."), paste("diet", cows_df$diet, sep = "."), sep = ":") == col_n
    cows_df[row_n, col_n] = 1
}

q3b_X = as.matrix(cows_df %>% mutate(intercept = 1) %>% select(intercept, starts_with("breed."), starts_with("diet."), contains(":")))

### c
q3_C = matrix(c(
    0, 0, 0, 1, -1, -1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 1, -1, 0, 0, -1, 1, 0, 0, 0
    ),
    nrow = 2, ncol = 12, byrow = TRUE
)
colnames(q3_C) = colnames(q3b_X)
identical(round(q3_C %*% ginv(t(q3b_X) %*% q3b_X) %*% t(q3b_X) %*% q3b_X, 10), q3_C)

### d
with(cows_df, interaction.plot(diet, breed, yield))
ggplot(data = cows_df, mapping = aes(x = diet, y = yield, colour = breed, group = breed)) +
    stat_summary(fun.y = mean, geom = "line") +
    stat_summary(fun.y = mean, geom = "point")

### e
n = dim(q3b_X)[1]
r = rankMatrix(q3b_X)[1]

b = ginv(t(q3b_X) %*% q3b_X) %*% t(q3b_X) %*% cows_df$yield
SSRes = t(cows_df$yield - q3b_X %*% b) %*% (cows_df$yield - q3b_X %*% b)
svar = SSRes / (n - r)

r1 = rankMatrix(q3_C)[1]

FStat = t(q3_C %*% b) %*% ginv(q3_C %*% ginv(t(q3b_X) %*% q3b_X) %*% t(q3_C)) %*% (q3_C %*% b) / r1 / svar

1 - pf(FStat, df1 = r1, df2 = (n - r))

q3_imodel = lm(yield ~ breed * diet, data = cows_df)
q3_amodel = lm(yield ~ breed + diet, data = cows_df)

### g)
q3_x_star = c(1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1)

t(q3_x_star) %*% b

### h)
b = ginv(t(q3a_X) %*% q3a_X) %*% t(q3a_X) %*% cows_df$yield
q3_x_star = c(1, 0, 1, 0, 0, 1)
t(q3_x_star) %*% b

### i)
q3_C = matrix(c(
    0, 0, 0, 0, 1, -1
    ),
    nrow = 1, ncol = 6, byrow = TRUE
)
colnames(q3_C) = colnames(q3a_X)
identical(round(q3_C %*% ginv(t(q3a_X) %*% q3a_X) %*% t(q3a_X) %*% q3a_X, 10), q3_C)

### j)
n = dim(q3a_X)[1]
r = rankMatrix(q3a_X)[1]
SSRes = c(t(cows_df$yield - q3a_X %*% b) %*% (cows_df$yield - q3a_X %*% b))
svar = SSRes / (n - r)

t_stat_975 = qt(0.975, df = n - r)

q3_x_star = c(1, 0, 1, 0, 0, 1)

c(q3_x_star %*% b) + c(-1, 1) * t_stat_975 * sqrt(svar) * c(sqrt(t(q3_x_star) %*% ginv(t(q3a_X) %*% q3a_X) %*% q3_x_star))

estimable(q3_amodel, c(1, 1, 0, 1), conf.int = 0.95)

### k)
estimable(q3_imodel, c(1, 1, 0, 1, 0, 1), conf.int = 0.95)