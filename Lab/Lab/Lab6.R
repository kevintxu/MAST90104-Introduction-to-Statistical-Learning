library(ggplot2)
library(plotly)
library(MASS)
library(car)
library(GGally)
library(Matrix)
library(lattice)
#library(devtools)
library(rgl)


## Q1
beef_df <- read.csv(file = "./beef.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")

ggpairs(beef_df)

q1_model_1 = lm(beef_df$yes ~ 1)
add1(q1_model, scope = ~. + beef_df$big + beef_df$live + beef_df$prin + beef_df$sale + beef_df$size + beef_df$val, test = "F")

q1_model_1 = lm(beef_df$yes ~ 1 + beef_df$size)
add1(q1_model, scope = ~. + beef_df$big + beef_df$live + beef_df$prin + beef_df$sale + beef_df$val, test = "F")

q1_model_3 = lm(beef_df$yes ~ 1)
q1_model_3 = step(q1_model_3, scope = ~. + beef_df$big + beef_df$live + beef_df$prin + beef_df$sale + beef_df$size + beef_df$val)

q1_model_4 = lm(beef_df$yes ~ 1)
q1_model_4 = step(q1_model_3, scope = ~. + beef_df$big + beef_df$live + beef_df$prin + beef_df$sale + beef_df$size + beef_df$val + (beef_df$size*beef_df$sale))

#py <- ggplotly(username = "kevintxu", key = "a6NgGeE3IVf7FpiuioLG")

q1_model_4$model = q1_model_4$model %>% arrange(`beef_df$size`)

x = q1_model_4$model$`beef_df$size`
y = order(q1_model_4$model$`beef_df$sale`)

persp3d(x = x, y = y,
    z = q1_model_4$coefficients["beef_df$size"] * x +
        q1_model_4$coefficients["beef_df$sale"] * y +
        q1_model_4$coefficients["beef_df$size:beef_df$sale"] * x * y)

## Q2
ggpairs(trees)

q2_y = trees$Volume

q2_X = cbind(1, trees$Girth, trees$Height)

q2_n = dim(q2_X)[1]
q2_p = dim(q2_X)[2]

q2_b = ginv(t(q2_X) %*% q2_X) %*% t(q2_X) %*% q2_y

SSRes = c(t(q2_y - q2_X %*% q2_b) %*% (q2_y - q2_X %*% q2_b))

C = matrix(c(0, 0, 1), 1, 3)
d_st = c(0)

Fstat = t(C %*% q2_b - d_st) %*% ginv(C %*% ginv(t(q2_X) %*% q2_X) %*% t(C)) %*% (C %*% q2_b - d_st) / (SSRes / (q2_n - q2_p))

pf(Fstat, rankMatrix(C), q2_n - q2_p, lower.tail = F)

q2_model1 = lm(Volume ~ Girth + Height, data = trees)
q2_model1_h0 = lm(Volume ~ Girth, data = trees)

anova(q2_model1_h0, q2_model1)
trees2 = trees
trees2$girth_sq = trees2$Girth * trees2$Girth
trees2[,"girth_sq_height"] = trees2$girth_sq * trees2$Height



q2_model2_full = lm(Volume ~ Girth + Height + girth_sq + girth_sq_height, data = trees2)

q2_model2 = step(q2_model2_full, scope = ~.)
q2_model2b = lm(log(Volume) ~ log(Height) + log(Girth), data = trees)

anova(q2_model2b, q2_model2)

plot(q2_model2b)

## Q9
q9_y = c(15, 26, 19, 15, 22, 26)

q9_X = matrix(
    c(1, 1, 0, 0,
        1, 1, 0, 0,
        1, 0, 1, 0,
        1, 0, 1, 0,
        1, 0, 0, 1,
        1, 0, 0, 1),
        nrow = 6, ncol = 4, byrow = T
    )

q9_b = ginv(t(q9_X) %*% q9_X) %*% t(q9_X) %*% q9_y

XtX = t(q9_X) %*% q9_X
XtXc = XtX
XtXc[1,] = 0
XtXc[, 1] = 0

XtXc[2:4, 2:4] = t(solve(XtXc[2:4, 2:4]))
XtXc = t(XtXc)

q9_b2 = XtXc %*% t(q9_X) %*% q9_y
