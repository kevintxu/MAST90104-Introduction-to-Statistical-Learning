library (dplyr)

## 1

### a) 

is_prime = Vectorize(function(n) {
    sqrt_n = sqrt(n)
    if (any(n %% (1:floor(sqrt_n))[-1] == 0)) { return (FALSE) }
    return (TRUE)
}
)

### b)
is_prime(101)
is_prime(153)

### c)
q1c = 3:1000
q1c_a = as.data.frame(cbind(x = q1c, is_prime = is_prime(q1c), is_row = 1))

is_prime(523)
is_prime(731)

### d)
plot(q1c_a$x, cumsum(q1c_a$is_prime) / cumsum(q1c_a$is_row), type="l")

### e)
q1e = 3:10000
q1e_a = as.data.frame(cbind(x = q1e, is_prime = is_prime(q1e), is_row = 1))


plot(q1e_a$x, cumsum(q1e_a$is_prime) / cumsum(q1e_a$is_row), type = "l")


## 2
data(swiss)
### a
q2_m = lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data = swiss)

summary(q2_m)

q2_m = lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data = swiss)

summary(q2_m)

### b)
plot(q2_m)

### c)
q2_m2 = step(q2_m)

### d)
plot(q2_m2)

## 3
motorins$Make = factor(motorins$Make)

### a)
q3_m = glm(Claims ~ Make + Insured, family = poisson(link = "log"), data = motorins)

### b)
q3_m2 = glm(Claims ~ Make * Insured, family = poisson(link = "log"), data = motorins)

### c)
anova(q3_m, q3_m2, test = "Chisq")


### d)
x_star = motorins[motorins$Make == 8,]
avg_8 = mean(motorins$Insured)
predict1 = predict(q3_m, newdata = list(Make = factor("8", levels = levels(motorins$Make)), Insured = avg_8), type = "response")
predict2 = predict(q3_m2, newdata = list(Make = factor("8", levels = levels(motorins$Make)), Insured = avg_8), type = "response")
