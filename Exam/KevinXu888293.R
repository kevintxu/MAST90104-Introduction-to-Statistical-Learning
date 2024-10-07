library (ggplot2)

## 2

### a)
state = data.frame(state.x77)

q2_model1 = lm (Life.Exp ~ Population + Income + Illiteracy + 
                  Murder + HS.Grad + Frost +Area  ,data = state)
summary (q2_model1)


### b)
plot(q2_model1)

### c)
q2_model2 = step(q2_model1)
summary(q2_model2)

### d)
anova(q2_model2, q2_model1)




## 3
library(readr)
spector <- read_csv("C:/Users/kevinx3/Downloads/spector.csv", 
                    locale = locale(tz = ""))
View(spector)

### a)
spector$New = factor(spector$New)
q3_model1 = glm(cbind(Improve, 1-Improve) ~ New + GPA, 
                family = binomial, data = spector)

summary(q3_model1)

### b)
q3_model2 = glm(cbind(Improve, 1-Improve) ~ New * GPA, 
                family = binomial, data = spector)

summary(q3_model2)

### c)
anova(q3_model1, q3_model2, test = "Chisq")

### d)
a_gpa = mean(spector$GPA)
x_star = data.frame (New = factor(c(1,0)), GPA = a_gpa)
x_star$pred = predict(q3_model1, newdata = x_star, type = "response")
x_star




## 1

### a)
q1_f = function(x, r, n, i) {
  r_vec = rep(0,n)
  r_vec[1] = x
  for (j in 2:n) {
    r_vec[j] = r * r_vec[j-1] * ( 1 - r_vec[j-1])
  }
  last_val = r_vec[(n-i+1):n]
  #p = plot (x = 1:n, y = r_vec, xlab = "j", ylab="xj")
  p = qplot ( x = 1:n, y = r_vec, xlab = "j", ylab="xj")
  return(list(x=r_vec, last_x = last_val, plot = p))

}

### b)
q1_ans1 = q1_f (x = 1/pi, r = 0.5, n = 4000, i = 10)
q1_ans1$plot
q1_ans2 = q1_f (x = 0.5, r = 0.5, n = 4000, i = 10)
q1_ans2$plot
q1_ans3 = q1_f (x = 3/pi, r = 0.5, n = 4000, i = 10)
q1_ans3$plot

### c)
q1_ans4 = q1_f (x = 1/pi, r = 2.5, n = 4000, i = 10)
q1_ans4$plot
q1_ans5 = q1_f (x = 0.5, r = 2.5, n = 4000, i = 10)
q1_ans5$plot
q1_ans6 = q1_f (x = 3/pi, r = 2.5, n = 4000, i = 10)
q1_ans6$plot


### d)
q1_ans7 = q1_f (x = 1/pi, r = 3.4, n = 4000, i = 10)
q1_ans7$plot
q1_ans8 = q1_f (x = 0.5, r = 3.4, n = 4000, i = 10)
q1_ans8$plot
q1_ans9 = q1_f (x = 3/pi, r = 3.4, n = 4000, i = 10)
q1_ans9$plot

### e)
q1_ans10 = q1_f (x = 1/pi, r = 4, n = 4000, i = 10)
q1_ans10$plot
q1_ans11 = q1_f (x = 0.5, r = 4, n = 4000, i = 10)
q1_ans11$plot
q1_ans12 = q1_f (x = 3/pi, r = 4, n = 4000, i = 10)
q1_ans12$plot