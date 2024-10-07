## function
func_name = function(args){
  # do something
  
  # flow control if-else
  if(condition 1){# then do somethng}
    else if(conditiuon 2){# then do something}
      else {# do something else}
        
  # for
  for(i in 1:n){#do}
          
  # while
  while(condition, e.g i <= n){#do something}
    
  return(ouput)
}
          
          
## math operation
+, -, *, /, %*%
floor(3.4) = 3
5%%3 = 2
round(4.3234,2) = 4.32
exp()
log()
sum()
prod()
factorial()
choose(n,k) n选k
sqrt()

# 3X^3 + 2X^2 + 6X +1
z = sum(x^(3:0)) * c(3,2,6,1)

## logical operation 
# single -pairwise for two vectors
# double -compare the first element of two vectors
True: T TRUE 
False: F FALSE 
AND: & &&
OR: | ||
          
## sequence and list
vec_name = c(e1,e2,e3..)
a = 1:n 
b = rep(the number you want, how many repeats)
seq_name = seq(start_num,end_num,step)

# Example:
# aa: 0-100之间所有不能被2， 3 ，7整除的数
a = 1:100
aa = a[(a %% 2 != 0) & (a %% 3 != 0) & (a %% 7 != 0)] 

## filter and map

# vector (==, >=, <=, !=)
sort(vec_name)
append()
# Example:
> test = c(1,2,3)
> sid = c(8,9)
> append(test,sid)
[1] 1 2 3 8 9
> append(sid,test)
[1] 8 9 1 2 3
> append(sid,test, after=1)
[1] 8 1 2 3 9

length(vec_name) 
vec_has_ele = ele %in% vec_name  # True or False
first_ele = match(ele,vec_name) # index or NA

# remove i-th ele in vec and update the vec, can be saved to a new vec
vec_name = vec_name[-i]
vect == some_value # filter, return logic value for all elements
vec_name[vec_name == some_value] # filter, return entries match the condition
which(vec_name == some_value ) # filter, return index of entries match the condition

# map opt1
vec_result = sapply(vec_name, func_name)

# map opt2
vec_func_name = Vectorize(func_name)
vec_result = vec_func_name(vec_name)



## For matrix X (row_num by col_num matrix)
X[i,j]: element at [row_i,col_j] 
# if use X[i], then call the i-th element in the list of X by column)
A = X[,1:3] 返回矩阵第1-3列,并存到矩阵A
X = X[，-（1:3）] 删除第1-3列，并更新原矩阵
X = X[,-c(1,3)] 删除第1和3列，并更新原矩阵

X = matrix(c(column_lists), row_num, col_num) # default byrow = F 
X = matrix(c(row_lists), row_num, col_num, byrow = TRUE)
dim(X) = row_num, col_num as a vector # dim(vec) = NULL
length(X) = dim(X)[1] = row_num
2*X # double all entries
X %*%　Y # mutiply matrix or vectors
det(X)
diag(3) # I3
diag(X) # diagonal of matrix X
#library(Matrix)
rankMatrix(X)[1] # rank
#library(MASS)
Xc = ginv(X)

sum(diag(X)) # trace
eigen(X)$values # eigen values
eigen(X)$vectors # eigen vectors

cbind(new_col, matrix) # first on the left
rbind(new_row, matrix) # first on the top
　
## file
# wrtie to a file
sink("output.txt") # all print-out will be in sink too
cat("TRUE")
cat("\n")
sink()
file.show("output.txt")

# read from file
# sep = " ";",";"\t";"-"
d = raed.table("output.txt", sep ="\n")

## data frame
# load data in R
data(ships)
# read data from csv, can make into a df, or use $ to call var
some_data = read.csv("file_name.csv") # set CWD before


# define empty df, use index [i,j] to call
df = data.frame(matrix(row_num,col_num)) 
# define df, can still call by index
df = data.frame("var1" = some_col, ..)
# all a column x with values from vec, can then rename it
some_df$x = vec_name 
colnames(some_df) = c( "col1", "col2"..)
rownames(some_df ) = c(...)


## simulation
sample(x, size, replace, prob)
runif(n) U(0,1) 取n个
# Example: 
sample(1:6, 100, replace = T, prob = c(0.1,0.1,0.1,0.1,0.1,0.5))
length(which(a==6)) 

ceiling(6*runif(1)) # dice
sample(1:6, replace = T)



## model selection
# data: X -a,b,c    y
# ~. all vars currently in given model
# default intercept， otherwise put y~ 0 + a + b + c
full_model = lm(y ~ a + b + c, data = data_name, )
drop1（fullmodel， scope = ~., test ="F") # drop big p
# from intercept only
null_model = lm(y ~ 1)
add1(null_model, scope = ~. + a + b + c, test = "F") # pick small p
# stepwise
# defualt steps = 1000, maximum num_steps before stop  
step(some_model, scope = ~. + some_var + some_var, steps = 1)


## factor & levels
data$varX = factor(data$varX)
# default order of levels are the order of levels are read from dataset
data$varX = factor(data$varX, levels = (a,b,c,d,e), ordered =T)
# over-write levels in factor(), and change how data is represented
levels = c("aa","bb","cc","others") 

## lm
model_name = lm( y ~ a + b + 0, data = some_data)

test = data.frame("a" = va, "b" = vb)
predict(model_name, test)
# return c(fitted_values, lower_bound, upper_bound)
# interval = "confidence"; "prediction"
# test = table with x_points, return a matrix of 3 columns
predict(model_name, test, interval = "confidence", level = 0.95)

b = some_model$coefficients
b = ginv(t(X) %*% X) %*% t(X) %*% y

H = X %*% ginv(t(X) %*% X) %*% t(X)
leverage = influence(some_model)$hat
leverage = hatvalues(some_model)

residuals = some_model$residuals
residuals = c(y - X %*% b)

SSRes = deviance(some_model)
SSRes = c(t(y - X %*% b) %*% (y - X %*% b))
SSRes = sum(residuals^2)
 
SSTotal = sum(y * y)
SSReg = SSTotal - SSRes
SSReg = c (t(X %*% b) %*% (X %*% b))

(n-p) = some_model$df.residual
s2 = deviance(some_model) / some_model$df.residual
s2 = SSRes / (n - p)

z_all_points = residuals / sqrt(s2 * (1-diag(H)))
z_all_points = rstandard(some_model)
 
cd = cooks.distance(some_model)

summary(some_model)
# p value in as for t-stat, square to get f -stat, testing 
# that beta_i = 0 or not
# residual standard error : s and df
# F-stat: corrected sum of sq, test model relevance for beta_1 - beta_k

## for less than full rank model:
# data: X -a,b    y
# ONE-WAY OR ONE-WAY + CONTINUOUS
X$a = factor(X$a) 
model = lm(y ~ b + a) # same slope, different intercept, coeff = mu_1, mu_2-mu_1 ...
mu = append(model$coefficients[1], c(model$coefficients[-2][2:9] + model$coefficients[1]))
sort(mu) 

# y~ b + a + a:b , WITH INTERATION
model2 = lm(y ~ b*a) # different slope, different intercept, coeff = mu_1, mu_2-mu_1 ...
mu = append(model2$coefficients[1], c(model2$coefficients[-2][2:9] + model2$coefficients[1]))
sort(mu)

# test = “Rao”; “LRT”; “Chisq”; “F”; “Cp”
# anova: compares two model, less -> full
anova(null_model, model, full_model, test = "F")
signif(vcov(some_model))  # s2 *(X.t %*% X)^(-1)

# hypothesis test
# library(car)
# can test if beta = c(value1, value2...)
# can test linear trandformation of b
# under default contr.treatment setting, can test if all mu_i = mu + t_i
# equal to each other by using matrix C that mu_i - mu_1 = 1
linearHypothesis(some_model, some_vec, some_value)
model <- lm(life ~ type, data = filters) # formula = only intercept(mu) + factor t1-t5
C = matrix(c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,5)
linearHypothesis(model,C,0)

## glm
# use the x variables to estimate the distribution parametrs
# e.g orings p(damage) ~ temperature
# eta = X.t %*% b
# dist.parameter = inv_link(eta)
glm(formula, data = some_data, family = some_family(link = some_link))
glm(cbind(num_success,total - num_success) ~ some_var, data = some_data,
  family = binomial(link = some_link))


# family and available links
# help: glm - family check links can be used for this family
- binomial(link = "logit")
- gaussian(link = "identity") # identity: no link function
- Gamma(link = "inverse")
- inverse.gaussian(link = "1/mu^2")
- poisson(link = "log")
- quasi(link = "identity", variance = "constant")
- quasibinomial(link = "logit")
- quasipoisson(link = "log")
- glm(formula, family = )

# predict dist parameter
predict.glm(some_model,dataPoints, type = "response")
# predict eta
predict.glm(some_model,dataPoints, type = "link")

# pearson's residual
pearson_res = residuals(some_model, type = "pearson")
pearson_SSres = sum(pearson_res^2)
phi_hat = pearson_SSres / some_model$df.residual

# jack-knifed residual
rstudent(some_model)

# Example: Orings
orings = read.csv('orings.csv')
modelo = glm(cbind(orings.damage,6-orings.damage)~orings.temp,data = orings, family = binomial)
eta = predict(modelo, data.frame("orings.temp" = 48), type = "link");eta = 1.28
t2 = modelo$family$linkinv(eta);t2 = 0.78 # if call link function then modelo$family$linkfun()
test = predict(modelo, data.frame("orings.temp" = 48), type = "response");test = 0.78

# anova
# test = “Rao”; “LRT”; “Chisq”; “F”; “Cp”
# anova: compares two model, less -> full, 
# p_value is small, reject, two models are different
# phi = 1, use Chisq (Bin, Poisson)
# use F for Gamma, Normal, Inverse-gaussian

# phi = 1 
anova(less_model, full_model, test = "Chisq")
# eq to
Da = model_a$deviance, df_a = model_a$df.residual
Db = model_b$deviance,  df_b = model_b$df.residual
pchisq(Da-Db, df_a - df_b, lower = F)

# phi /= 1
phi_hat_b = pearson_SSres_b / model_b$df.residual
F_stat = [ (Da-Db) / (df_a - df_b) ] / phi_hat_b
pf(F_stat, (df_a - df_b), model_b$df.residual, lower = F)

# model adequacy
# phi = 1, use Chisq
anova(model, test = "Chisq")
# p-value is large, not reject, no evidence of model inadequacy
# eq to
D = model$deviance
D_null = model_a$null.deviance,  df_null = model_a$df.null
df = model$df.residual
pchisq(D_null-D, df_null-df, lower = F)

# phi /= 1, use F
anova(model, test = "F")

## multinomial and ordinal

# ordinal
# levels = increase order
data$varX = factor(data$varX, levels = (a,b,c,d,e), ordered =T)
# over-write levels in factor(), and change how data is represented
levels = c("aa","bb","cc","others") 

# fit multinomial
# library(nnet)

# 1. read data
# 2. var_name = factor(var_name, levels = c(...), 
#           ordered  = T or F)
#    if numerical data, bin 
# 3. have no freq, use opt_1; 
#    freq_table, use opt_2;
#    x_tab, use opt_3

# opt_1
some_model = multinom(y_cat ~ x_cat + x_cat2, data = some_data) 
# opt_2
some_model = multinom(y_cat ~ x_cat + x_cat2, weights = Freq_col_name, data = some_data) 
# opt_3
some_xtab = xtabs (Freq_col_name ~ x_cat + x_cat2, data = some_data )
some_model = multinom (t(some_xtab)~ x_cat + x_cat2)

predict(model, dataPoints, type = "probs") # otherwise, return hihest


# fit ordinal data
# library(MASS)
# usage same as multinom

# 1. read data
# 2. factor(levels = c()), 必须对y排序， x如果有顺序也排序
#    
# 3. have no freq, use opt_1; 
#    freq_table, use opt_2;
#    x_tab, use opt_3

some_model = polr() 




# plot
# 
plot(model) # which = c(1,2,3,5)

# plot a function
y_func = function(x, a = 4){return(x+a)} # x first
x.s = seq(-2,2, by = 0.1)
y.s = sapply(x.s, y_func)
plot(x.s, y.s)

# ggplot
# library(ggplot2)
# Example 01:

# factor factor first, or the range of numerical factor can be a color bar
rdc = read.csv('Radioactive.csv')
colnames(rdc)[1] = 'Counts'
plot_2a = qplot(Time, log(Counts), data = rdc, color = Material, shape = Sample) + scale_shape_manual(values = c(16,25)) + coord_cartesian(ylim = c(2,8))
plot_2a
# add a function
plot_2b = plot2a + stat_function(fun = y_func, args=list(other_param1_than_x = ..,param2 = ..), color = "red")

# fit a model, do prediction and draw a line
model_2b_1 = glm(Counts~Time*Material*Sample, family = poisson, data = rdc_new)

Counts_2b_1 = predict(model_2b_1, rdc_new %>% dplyr::select(Time,Material,Sample), type = 'link')
plot_2b_1 = plot_2a + geom_line(mapping = aes(x = Time, y = Counts_2b_1, color = Material, linetype = Sample), data = rdc_new)


# Example 02:
fbs = read.csv('Forbes500.csv')
qplot(logAssets, logSales, data = fbs, color = sector)


# plot data point
plot(x,y)
abline(a = coef1, b = coef2)

# plot lots point to approximate a line
plot(x, y , type = "l", xlab = some_label, ylab = some-label, xlim = c(-5,5), ylim = c(-10,10))






