library(ggplot2)

x = -20:20
b = c(1, 0.5)

y_fun = function(x) {
  return(b[1] + b[2]*x)
}

y = y_fun(x)

logit_inv = function (y) {
  return(1/(1+exp(-y)))
}

logit = function (p) {
  return(log(p/(1-p)))
}

log.log_inv = function (y) {
  return(1-exp(-exp(y)))
}

log.log = function (p) {
  return(log(-log(1-p)))
}
 
df1 = data.frame(var1=x, res=y)

plot1 = ggplot(data=df1, mapping = aes(x=var1, y=res)) + geom_line()
plot1

df2 = data.frame(var1=x, res=logit_inv(y), link="Logit")
df3 = data.frame(var1=x, res=log.log_inv(y), link="Log Log")
df4 = data.frame(var1=x, res=pnorm(y), link="Probit")
df = rbind(df2, df3, df4)
plot2 = ggplot(data=df, mapping = aes(x=var1, y=res)) + geom_line(mapping=aes(colour=link))
plot2
