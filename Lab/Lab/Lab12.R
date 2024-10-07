# seed position 1 
# set.seed(7) 
mu <- rep(0, 6)
for (i in 1:6) {
    # seed position 2 
    set.seed(7)
    X <- rep(0, 1000)
    for (j in 1:1000) {
        # seed position 3 
        #set.seed(7)
        X[j] <- runif(1)
    }
    mu[i] <- mean(X)
}
spread <- max(mu) - min(mu)
mu.estimate <- mean(mu)

## Question 9
library(ggplot2)
library(ggfortify)
irisscale <- scale(iris[c(1, 2, 3, 4)])
irispc <- prcomp(irisscale, retx = TRUE)
summary(irispc)
irispc$rotation