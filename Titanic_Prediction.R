library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)


options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42,sample.kind="Rounding")
ind <- createDataPartition(titanic_clean$Survived,times=1,p=0.2,list=F)
test <- titanic_clean[ind,]
train<- titanic_clean[-ind,]

set.seed(3,sample.kind="Rounding")
guess<-sample(c(0,1),nrow(test),replace=TRUE)

mean(test$Survived==guess)

train %>% group_by(Sex) %>%
  summarize(mean(Survived == 1))
train %>% filter(Sex=="female") %>% summarize(mean(Survived==1))

female_guess <- ifelse(test$Sex=="female",1,0)
mean(female_guess==test$Survived)

titanic_clean %>% group_by(Pclass) %>% summarize(mean(Survived==1))
class_guess <- ifelse(test$Pclass==1,1,0)
mean(class_guess==test$Survived)

titanic_clean %>% group_by(Pclass,Sex) %>% summarize(mean(Survived==1))

female_rich_guess <- ifelse(test$Sex=="female"& test$Pclass %in% c(1,2),1,0) 
mean(female1_guess==test$Survived)

confusionMatrix(as.factor(female_guess),test$Survived)
confusionMatrix(as.factor(class_guess),test$Survived)
confusionMatrix(as.factor(female_rich_guess),test$Survived)

F_meas(test$Survived,as.factor(female_guess))
F_meas(test$Survived,as.factor(class_guess))
F_meas(test$Survived,as.factor(female_rich_guess))


#Linear and Quadratic discriminant Analysis
set.seed(1,sample.kind="Rounding")
(lmodel <- train(Survived~Fare,train,method="lda"))
(qmodel <- train(Survived~Fare,train,method="qda"))
mean(predict(lmodel,test)==test$Survived)
mean(predict(qmodel,test)==test$Survived)

#Logistic Regression
set.seed(1,sample.kind="Rounding")
gmodel <- train(Survived~Age,train,method="glm",family=binomial)
mean(predict(gmodel,test,type="raw")==test$Survived)

gmodel_ <- train(Survived~Sex+Age+Pclass+Fare,train,method="glm",family=binomial)
mean(predict(gmodel_,test,type="raw")==test$Survived)

#No difference when using all predictors
gmodel <- train(Survived~.,train,method="glm",family=binomial)
mean(predict(gmodel,test,type="raw")==test$Survived)

#K Nearest Neighbors
set.seed(6,sample.kind="Rounding")
kmodel <- train(Survived~.,data=train,method="knn",tuneGrid=data.frame(k=seq(3,51,2)))
kmodel$finalModel
kmodel$bestTune
ggplot(kmodel)
mean(predict(kmodel,test)==test$Survived)


#Using Cross Validation
set.seed(8,sample.kind="Rounding")
kcvmodel <- train(Survived~.,data=train,
                  method="knn",
                  tuneGrid=data.frame(k=seq(3,51,2)),
                  trControl=trainControl(method="cv",number=10,p=0.9))
kcvmodel$finalModel
kcvmodel$bestTune
ggplot(kcvmodel)
mean(predict(kcvmodel,test)==test$Survived)

#Decision tree rpart
set.seed(10,sample.kind="Rounding")
rmodel <- train(Survived~.,
                  data=train,
                  method="rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
rmodel$finalModel
rmodel$bestTune
plot(rmodel$finalModel, margin = 0.1)
text(rmodel$finalModel, cex = 0.75)
mean(predict(rmodel,test)==test$Survived)

#Random forest

set.seed(14,sample.kind="Rounding")
rfmodel <- train(Survived~.,
                data=train,
                method="rf",
                tuneGrid = data.frame(mtry=seq(1,7)),
                ntree=100)
rfmodel$finalModel
rfmodel$bestTune
mean(predict(rfmodel,test)==test$Survived)

#Most important predictors
varImp(rfmodel)
