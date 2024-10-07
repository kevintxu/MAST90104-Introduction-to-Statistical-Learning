library(ggplot2)
library(plotly)
library(MASS)
library(car)


mammals <- read.csv(file = "C:/Users/Kevin/OneDrive - The University of Melbourne/MAST90104 Introduction to Statistical Learning/Lab/Lab/sleep.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "", na.strings = "NA")
row.names(mammals) = mammals$Species
mammals$BodyWt = log(mammals$BodyWt)
mammals$BrainWt = log(mammals$BrainWt)

## 1
mammals_model = lm(BrainWt ~ BodyWt, data = mammals)
pl = ggplot(data = mammals, mapping = aes(x = BodyWt, y = BrainWt)) +
    geom_point() +
    geom_smooth(method = "lm")
ggplotly(pl)


## 2
### a)
b=c(mammals_model$coefficients)
X = cbind(1, mammals$BodyWt)
y = mammals$BrainWt
H = hatvalues(mammals_model)
H = X %*% ginv(t(X) %*% X) %*% t(X)
b = ginv(t(X) %*% X) %*% t(X) %*% y
n = dim(X)[1]
p = dim(X)[2]


### b)
e = mammals_model$residuals
e = c(y - X %*% b)

### c
SSRes = deviance (mammals_model)
SSRes = c(t(y - X %*% b) %*% (y - X %*% b))

### d
SSTotal = sum(y * y)

SSReg = SSTotal - SSRes
SSReg = c (t(X %*% b) %*% (X %*% b))

### e
svar = deviance(mammals_model)/mammals_model$df.residual
svar = SSRes / (n - p)

### f
h = diag(H)
h = hatvalues(mammals_model)
z = e / sqrt(svar * (1 - diag(H)))
z = rstandard(mammals_model)
z = stdres(mammals_model)

### g
leverage = hatvalues(mammals_model)
leverage = diag(H)

### h
cd = cooks.distance(mammals_model)
cd = 1 / p * z ^ 2 * (leverage / (1 - leverage))

## 3
ci = predict(mammals_model, data.frame(BodyWt = log(50)), interval = "confidence", level = 0.95)
t_95 = qt(0.975, df = n-p)
x_s = matrix(c(1, log(50)))
ci = c(t(x_s) %*% b) + (t_95 * sqrt(svar) * c(sqrt(t(x_s) %*% ginv(t(X) %*% X) %*% x_s)) * c(-1, 1))

## 4
pi = predict(mammals_model, data.frame(BodyWt = log(50)), interval = "prediction", level = 0.95)
pi = c(t(x_s) %*% b) + (t_95 * sqrt(svar) * c(sqrt(1 + t(x_s) %*% ginv(t(X) %*% X) %*% x_s)) * c(-1, 1))
c = ginv(t(X)%*%X)

bci = cbind(b - (t_95 * sqrt(svar) * sqrt(diag(c))), b + (t_95 * sqrt(svar) * sqrt(diag(c))))

## 5
### a)
q5a_null_model = lm(BrainWt ~ 0, data = mammals)
anova(q5a_null_model, mammals_model)
b_s = c(0,0)
1 - pf(c(t(b - b_s) %*% t(X) %*% X %*% (b - b_s) / (p*svar)), df1 = p, df2 = n - p)


### b)
q5b_null_model = lm(BrainWt ~ 1, data = mammals)
anova(q5b_null_model, mammals_model)
C = matrix(c(0,0,0,1), 2, 2) 
b_s = c(0, 0)
1 - pf(c(t(C%*%b - b_s) %*% ginv(C %*% ginv(t(X) %*% X) %*% t(C)) %*% (b - b_s) / 1)/(SSRes/(n-p)), df1 = p, df2 = n - p)

### c)
q5c_null_model = lm(BrainWt ~ 0, data = mammals)
anova(q5c_null_model, mammals_model)
C = matrix(c(1, 0, 0, 0), 2, 2)
b_s = c(0, 0)
1 - pf(c(t(C %*% b - b_s) %*% ginv(C %*% ginv(t(X) %*% X) %*% t(C)) %*% (b - b_s) / 1) / (SSRes / (n - p)), df1 = p, df2 = n - p)


### d)
C = diag(2)
b_s = c(2,1)
linearHypothesis(mammals_model, C, b_s)
1 - pf(c(t(C %*% b - b_s) %*% ginv(C %*% ginv(t(X) %*% X) %*% t(C)) %*% (b - b_s) / 2) / (SSRes / (n - p)), df1 = p, df2 = n - p)


summary(mammals_model)
mammals_model_bigger = lm(BrainWt ~ BodyWt + TotalSleep, data = mammals)
summary(mammals_model_bigger)