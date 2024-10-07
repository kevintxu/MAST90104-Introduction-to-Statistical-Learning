library(faraway)
library(nnet)
library(MASS)
library(tidyverse)

## 1
data(hsb)

rownames(hsb) = hsb$id

hsb_df = hsb %>% mutate(ses.ordered = factor(ses, levels = c("low", "middle", "high"), ordered = TRUE)) %>%
    select(gender, race, ses.ordered, schtyp, read, write, math, science, socst, prog)
rownames(hsb_df) = hsb$id

### a
q1_model_full = multinom(prog ~ gender + race + ses + schtyp + read + write + math + science + socst, data = hsb)

### b
q1_model_aic = step(q1_model_full)

summary(q1_model_aic)

#q1_model = multinom(prog ~ ses + schtyp + math + science + socst, data = hsb)
q1_model = multinom(prog ~ schtyp + math + science + socst, data = hsb)
anova(q1_model, q1_model_aic)

q1_model = multinom(prog ~ ses + math + science + socst, data = hsb)
anova(q1_model, q1_model_aic)

q1_model = multinom(prog ~ ses + schtyp + science + socst, data = hsb)
anova(q1_model, q1_model_aic)

q1_model = multinom(prog ~ ses + schtyp + math + socst, data = hsb)
anova(q1_model, q1_model_aic)

q1_model = multinom(prog ~ ses + schtyp + math + science, data = hsb)
anova(q1_model, q1_model_aic)

### c


## 2
data(pneumo)

### a
q2_model_full = multinom(status ~ year, weights = Freq, data = pneumo)

predict(q2_model_full, list(year=25), type="probs")

### b
pneumo$status = factor(pneumo$status, levels = c("normal", "mild", "severe"), ordered = TRUE)
q2_model_ordered = polr(status ~ year, weights = Freq, data = pneumo)

predict(q2_model_ordered, list(year = 25), type = "probs")

## 3
data(nes96)

nes96$sPID <- nes96$PID
# recode party affiliation as Republican, Democrat or Independent
levels(nes96$sPID) <- c("Democrat", "Democrat", "Independent", "Independent",
"Independent", "Republican", "Republican")


# recode income as midpoint of group to make it numerical
inca <- c(1.5, 4, 6, 8, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 16, 18.5, 21, 23.5,
27.5, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 82.5, 97.5, 115)
nes96$nincome <- inca[unclass(nes96$income)]

omod <- polr(sPID ~ age + educ + nincome, nes96)
omod = step(omod)

table(nes96$nincome, nes96$sPID)
obs = prop.table(table(nes96$nincome, nes96$sPID), 1)

p1 <- obs[, 1]
p2 <- obs[, 1] + obs[, 2]

log_o1 <- log(p1 / (1 - p1))
log_o2 <- log(p2 / (1 - p2))

income_4 = matrix(c(4))
income_6 = matrix(c(6))

exp(t(income_6 - income_4) %*% matrix(omod$coefficients))

(obs["4", 1] / (1 - obs["4", 1])) / (obs["6", 1] / (1 - obs["6", 1]))
