library(survival)
library(faraway)
library(MASS)
library(tidyverse)
library(plotly)
library(GGally)
library(gghalfnorm)

## 1
data(discoveries)
year = 1860:1959
year2 = year^2

model_full = glm(discoveries ~ year + year2, family = poisson(link = "log"))
model = glm(discoveries ~ 1, family = poisson(link = "log"))

anova(model, model_full, test = "Chisq")
1 - pchisq(deviance(model) - deviance(model_full), 99 - 97)

summary(model_full)

## 2
data(ships)
model_full = glm(ships$incidents ~ type + year + period + service, family = poisson(link = "log"), data = ships)
model = glm(ships$incidents ~ year + period + service, family = poisson(link = "log"), data = ships)

anova(model, model_full, test = "Chisq")
1 - pchisq(deviance(model) - deviance(model_full), 36 - 32)


summary(model_full)

## 3
data(infert)
infert$education1 = 1
infert$education1[infert$education == "0-5yrs"] = 0
infert$education1 = factor(infert$education1)

model_full = glm(cbind(case, 1 - case) ~ education1 + age + parity + induced + spontaneous, family = binomial(link = "logit"), data = infert)

summary(model_full)



model1 = glm(cbind(case, 1 - case) ~ education + age + parity + induced + spontaneous, family = binomial(link = "logit"), data = infert)
deviance(model1) - deviance(model_full)
summary (model1)

model2 = glm(cbind(case, 1 - case) ~ 0 + education + parity + induced + spontaneous, family = binomial(link = "logit"), data = infert)
deviance(model2) - deviance(model1)
summary (model2)
1- pchisq(deviance(model2) - deviance(model1), 242 - 241)

model3 = glm(cbind(case, 1 - case) ~ 0 + parity + induced + spontaneous, family = binomial(link = "logit"), data = infert)
deviance(model3) - deviance(model2)
summary (model3)
1- pchisq(deviance(model3) - deviance(model2), 245 - 242)

step(model)

## 4
data(africa)

model_full = glm(miltcoup ~ oligarchy + pollib + parties + pctvote + popn + size + numelec + numregim, family = poisson("log"), data = africa)

model = step(model_full)

model = glm(miltcoup ~ oligarchy + pollib + parties, family = poisson("log"), data = africa)

model = step(model)

Res = residuals(model, type = "pearson")

SSRes = sum(Res ^ 2)

SSRes / (dim(africa)[1] - 4)

## 5
data(cornnit)

pairs(cornnit)

cornnit$nitrogen[cornnit$nitrogen == 0] = 0.0000001

cornnit$log_nitrogen = log(cornnit$nitrogen)

#model1 = glm(yield ~ nitrogen, family = Gamma(link = "inverse"), data = cornnit)
#model2 = glm(yield ~ nitrogen, family = gaussian(link = "identity"), data = cornnit)
#model3 = glm(yield ~ nitrogen, family = Gamma(link = "identity"), data = cornnit)
#model4 = glm(yield ~ nitrogen, family = Gamma(link = "log"), data = cornnit)

model1 = glm(yield ~ log_nitrogen, family = Gamma(link = "inverse"), data = cornnit)
model2 = glm(yield ~ log_nitrogen, family = gaussian(link = "identity"), data = cornnit)
model3 = glm(yield ~ log_nitrogen, family = Gamma(link = "identity"), data = cornnit)
model4 = glm(yield ~ log_nitrogen, family = Gamma(link = "log"), data = cornnit)

model1$aic
model2$aic
model3$aic
model4$aic

x_star = data.frame(nitrogen = seq(0, 300, 10))
x_star$log_nitrogen = log(x_star$nitrogen + 0.0000001)

y1_star = predict(model1, newdata = x_star, type = "response", se = TRUE)
y2_star = predict(model2, newdata = x_star, type = "response", se = TRUE)
y3_star = predict(model3, newdata = x_star, type = "link", se = TRUE)
y4_star = predict(model4, newdata = x_star, type = "link", se = TRUE)


#plot = ggplot() + geom_point(mapping = aes(x = nitrogen, y = yield), data = cornnit) +
    #geom_line(mapping = aes(x = x_star$nitrogen, y = y1_star$fit), colour = "green4") +
    #geom_line(mapping = aes(x = x_star$nitrogen, y = model3$family$linkinv(y3_star$fit)), colour = "green3") +
    #geom_line(mapping = aes(x = x_star$nitrogen, y = model4$family$linkinv(y4_star$fit)), colour = "green2") +
    #geom_line(mapping = aes(x = x_star$nitrogen, y = y2_star$fit), colour = "chocolate")
#ggplotly(plot)

plot = ggplot() + geom_point(mapping = aes(x = log_nitrogen, y = yield), data = cornnit) +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = y1_star$fit), colour = "green4") +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = model3$family$linkinv(y3_star$fit)), colour = "green3") +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = model4$family$linkinv(y4_star$fit)), colour = "green2") +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = y2_star$fit), colour = "chocolate")
ggplotly(plot)

Res = residuals(model1, type = "pearson")

### b)
model_null = glm(yield ~ 1, family = Gamma(link = "identity"), data = cornnit)

SSRes = sum(Res^2)

svar = SSRes / (dim(cornnit)[1] - 2)

anova(model_null, model1, test = "F")
Fstat = (deviance(model_null) - deviance(model1)) / (1) / svar
Fstat

1 - pf(Fstat, df1 = 1, df2 = dim(cornnit)[1] - 2)

anova(model_null, model1, test = "Chisq")
1 - pchisq((deviance(model_null) - deviance(model3))/svar, df.residual(model_null) - df.residual(model3)) ## need to divide by a scale factor (ie. svar)

### c)
plot(model1)
# leverages
gghalfnorm(influence(model1)$hat, ylab = "leverages")

# jackknife residuals
gghalfnorm(rstudent(model1), ylab = "jackknifed resdiuals")

# Cook's distance - obs 25 looking influential
gghalfnorm(cooks.distance(model1), ylab = "cooks.distance")

### d)
model5 = lm(yield ~ log_nitrogen,  data = cornnit)

model1$aic
model2$aic
model3$aic
model4$aic
step(model5)

y5_star = predict(model5, newdata = x_star, se = TRUE)

plot = ggplot() + geom_point(mapping = aes(x = log_nitrogen, y = yield), data = cornnit) +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = y1_star$fit), colour = "green4") +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = model3$family$linkinv(y3_star$fit)), colour = "green3") +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = model4$family$linkinv(y4_star$fit)), colour = "green2") +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = y2_star$fit), colour = "chocolate") +
    geom_line(mapping = aes(x = x_star$log_nitrogen, y = y5_star$fit), colour = "blue")
ggplotly(plot)