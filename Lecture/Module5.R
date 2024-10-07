library(readr)
library(ggplot2)
library(plotly)
system_cost <- read_csv("C:/Users/kevin/OneDrive - The University of Melbourne/MAST90104 Introduction to Statistical Learning/Lecture/system_cost.csv")

X = cbind(1, system_cost$Files, system_cost$Flows, system_cost$Processes)
y = system_cost$Cost
b <- c(solve(t(X) %*% X, t(X) %*% y))
n = dim(X)[1]
p = dim(X)[2]

(SSReg <- t(y)%*%X%*%b)

(SSTotal <- sum(y^2))

(SSRes <- SSTotal - SSReg)

(MSReg <- SSReg/p)

(MSRes <- SSRes/(n-p))

(Fstat <- MSReg/MSRes)


delta_st <- b
C <- diag(4)
r <- qr(C)$rank
num <- t(C %*% b - delta_st) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% (C %*% b - delta_st) / r
Fstat <- num/(SSRes/(n-p))

pf(Fstat, r, n-p, lower=F)


bst <- c(2,0,1)
C <- matrix(c(1,0,0,0,0,1,-1,0,0,0,0,1),3,4, byrow = TRUE)
r <- 4
n= dim(X)[1]
p = dim(X)[2]
num <- t(C%*%b-bst)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%b-bst)/r
Fstat <- num/(SSRes/(n-p))

pf(Fstat, r, n-p, lower=F)

dst <- c(0, 0)
C <- matrix(c(0, 0, 1, 0, 0, 1, -1, -1), 2, 4)
r <- 2
num1 <- t(C %*% b - dst) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*%
(C %*% b - dst) / r
(Fstat1 <- num1 / (SSRes / (n - p)))
## [,1]
## [1,] 5.785777
pf(Fstat, r, n - p, lower = F)

model_null = lm(Cost ~ 1, data = system_cost)
model_null_uncorrected = lm(Cost ~ 0, data = system_cost)

model_full = lm(Cost ~ 1 + Files + Flows + Processes, data = system_cost)

model_sub = step(model_full, scope = ~.)

anova(model_null, model_sub, model_full)

anova(model_null_uncorrected, model_sub, model_full)