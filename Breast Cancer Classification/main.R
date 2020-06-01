options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(gam)
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

#Create train,test sets
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y=="B")
mean(test_y=="B")

#kmeans
set.seed(3,sample.kind="Rounding")
k <- kmeans(train_x, centers = 2)

#Assigns clusters to observations from x using a kmeans object k
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}


#replace the clusters with B and M respectively
preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
preds
(acc_kmeans <- mean(preds==test_y))

#Confusion of preds and actual B,M
confusionMatrix(as_factor(preds),test_y)

#Logistic Regression
lm <- train(y = train_y,x = train_x, method="glm", family = binomial)
mean(predict(lr,test_x) == test_y)

#Linear Discriminant Analysis
ld <- train(y = train_y,x = train_x, method="lda")
mean(predict(ld,test_x) == test_y)

#Quadratic Discriminant Analysi
qd <- train(y = train_y,x = train_x, method="qda")
mean(predict(qd,test_x) == test_y)

#Loess
loess <- train(train_x, train_y,
                     method = "gamLoess")
mean(predict(loess, test_x) == test_y)
