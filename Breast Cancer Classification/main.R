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

#Prinicipal Component Analysis for dimensionality reduciton
pc <- prcomp(x_scaled)
#biplot(pc)
summary(pc)$importance[3,]
plot(summary(pc)$importance[3,])

#Plotting Malignant and Benign types for PC1 and PC2
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], tumor = brca$y) %>%
  ggplot(aes(pc_1, pc_2, color = tumor)) +
  geom_point() +
  theme(legend.position="top")

#boxplots of PC 1:10
for(i in 1:10){
  boxplot(pc$x[,i] ~ brca$y, main = paste("PC", i))
}

#All in one using gather
data.frame(type = brca$y, pc$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()
