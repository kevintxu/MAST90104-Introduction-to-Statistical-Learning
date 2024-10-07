library(tidyverse)
library(GGally)
library(plotly)
library(faraway)
data(wbca)


## 1
model_logit = glm(cbind(Class, 1 - Class) ~ Adhes + BNucl + Chrom + Epith + Mitos + NNucl + Thick + UShap + USize, family = binomial(link = "logit"), data = wbca)

model_logit_reduced = step(model_logit, scope = ~.)

new_data = data.frame(Adhes = 1, BNucl = 1, Chrom = 3, Mitos = 1, NNucl = 1, Thick = 4, UShap = 1)
fit_p = predict(model_logit_reduced, new_data, type = "response", se.fit = TRUE)
fit_p$fit + c(-1, 1) * qnorm(0.975) * fit_p$se.fit

fit_lp = predict(model_logit_reduced, new_data, type = "link", se.fit = TRUE)
ilogit(fit_lp$fit + c(-1, 1) * qnorm(0.975) * fit_lp$se.fit)

test_data = data.frame(Adhes = wbca$Adhes, BNucl = wbca$BNucl, Chrom = wbca$Chrom, Mitos = wbca$Mitos, NNucl = wbca$NNucl, Thick = wbca$Thick, UShap = wbca$UShap)
cancer_p = predict(model_logit_reduced, test_data, type = "response")
result = data.frame(Actual = wbca$Class, Predicted = round(cancer_p), raw_prediction = round(cancer_p,3))
result$Match = result$Actual == result$Predicted
result$false_pos = result$Actual == 0 & result$Predicted == 1
result$false_neg = result$Actual == 1 & result$Predicted == 0
result %>% summarise_at(.cols = c("Match"), funs(TotalMatch = sum))

result2 = data.frame(Actual = wbca$Class, Predicted = ifelse(cancer_p > 0.9, 1, 0) , raw_prediction = round(cancer_p, 3))
result2$Match = result2$Actual == result2$Predicted
result2$false_pos = result2$Actual == 0 & result2$Predicted == 1
result2$false_neg = result2$Actual == 1 & result2$Predicted == 0
result2 %>% summarise_at(.cols = c("Match"), funs(TotalMatch = sum))

## 2
data(pima)
ggplotly(ggpairs(pima))