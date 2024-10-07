library(dplyr)
library(ggplot2)
library(plotly)
library(MASS)


y <- c(39.5, 47.4, 31.2, 44)
X <- matrix(c(
    1, 1, 0, 1, 0,
    1, 1, 0, 0, 1,
    1, 0, 1, 1, 0,
    1, 0, 1, 0, 1
    ),4, 5, byrow = TRUE)
n <- 4
r <- 3
b <- ginv(t(X) %*% X) %*% t(X) %*% y
s2 <- sum((y - X %*% b) ^ 2) / (n - r)
(C <- matrix(c(0, 1, -1, 0, 0), 1, 5))

round(C %*% ginv(t(X) %*% X) %*% t(X) %*% X, 6)


C <- matrix(0, 4, 16)
C[1, c(8, 9, 11, 12)] <- c(1, -1, -1, 1)
C[2, c(9, 10, 12, 13)] <- c(1, -1, -1, 1)
C[3, c(11, 12, 14, 15)] <- c(1, -1, -1, 1)
C[4, c(12, 13, 15, 16)] <- c(1, -1, -1, 1)
C[, - (1:7)]

math_df = read.csv("./maths.csv")

math_df$class.f = factor(math_df$class)
model_a <- lm(maths.y ~ iq + class.f, data = math_df)
summary(model_a)

model_i <- lm(maths.y ~ iq * class.f, data = math_df)
summary(model_i)

anova(model_a, model_i)

plot=ggplot(data=math_df, mapping=aes(x=iq, y=maths.y,color=class.f))+geom_point()
plot

summary(model_i)


model_1 <- lm(maths.y ~ iq, data = math_df[math_df$class==1,])
model_2 <- lm(maths.y ~ iq, data = math_df[math_df$class==2,])
model_3 <- lm(maths.y ~ iq, data = math_df[math_df$class==3,])

summary(model_1)
summary(model_2)
summary(model_3)
