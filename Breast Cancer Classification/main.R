options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(matrixStats)
data(brca)

#Predictors
dim(brca$x)

#Proportion of malignant
mean(brca$y == "M")

which.max(colMeans(brca$x))
which.min(colSds(brca$x))

#Normalize
x_scaled <- sweep(brca$x,2,colMeans(brca$x),"-")
x_scaled <- sweep(x_scaled,2,colSds(brca$x),"/")
colSds(x_scaled)
median(x_scaled[,1])

#Distnace matrix for the predictors:x
dm <- as.matrix(dist(x_scaled))

#Benign samples
b <- which(brca$y=="B")
#Malignant samples
m <- which(brca$y=="M")

#Average distance from first sample
mean(dm[b,1])
mean(dm[m,1])

#Heatmap of distance relationship between predictors.
#We transpose x_scaled as we want the distances between the 30 predictors/features rather than the samples.
d_pred <- dist(t(x_scaled))
heatmap(as.matrix(d_pred), labRow = NA, labCol = NA)  

#Heirarchical clustering of features
h <- hclust(d_pred)
plot(h)

#prune the tree to only 5 clusters
(clusters <- cutree(h, k = 5))
names(clusters)[clusters == 3]
#Features seggreegated into different clusters.
split(names(clusters),clusters)





