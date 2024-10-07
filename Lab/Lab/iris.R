library(ggplot2)
library(ggfortify)
# set up data set and do principal components with output
irisscale <- scale(iris[c(1, 2, 3, 4)])
irispc <- prcomp(irisscale, retx = TRUE)
summary(irispc)
irispc$rotation
# choose cluster number using total within cluster sum of squares
library(cluster)
k.max <- 6 # Maximal number of clusters
wss <- sapply(1:k.max,
              function(k) { kmeans(irisscale, k, nstart = 3)$tot.withinss })
              plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
abline(v = 3, lty = 2)
# choose cluster number using average silohouette 
library(factoextra)
library(NbClust)
library(fpc)
fviz_nbclust(irisscale, kmeans, method = c("silhouette"))
#  find  2 clusters and plot
Kiris <- kmeans(irisscale, 2)
autoplot(Kiris, data = irisscale)
#  compute the cluster menas by selecting the x values 
# in the principal component outputs
x1 <- mean(irispc$x[, 1][Kiris$cluster == 1])
y1 <- mean(irispc$x[, 2][Kiris$cluster == 1])
x2 <- mean(irispc$x[, 1][Kiris$cluster == 2])
y2 <- mean(irispc$x[, 2][Kiris$cluster == 2])
# scale = 0 plots the principal components in the units of the data
autoplot(irispc, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0) +
         # plot the cluster means and identifying text
         geom_point(x = x1, y = y1, colour = "black") +
         geom_text(label = "cluster 1 mean", x = x1, y = y1,
          colour = "black", size = 3, hjust = -0.1, vjust = 0.2) +
          geom_point(x = x2, y = y2, colour = "black") +
          geom_text(label = "cluster 2 mean", x = x2, y = y2,
            colour = "black", size = 3, hjust = -0.1, vjust = 0.2)
