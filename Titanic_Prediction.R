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

female1_guess <- ifelse(test$Sex=="female"& test$Pclass %in% c(1,2),1,0) 
mean(female1_guess==test$Survived)

confusionMatrix(as.factor(female_guess),test$Survived)
confusionMatrix(as.factor(class_guess),test$Survived)
confusionMatrix(as.factor(female1_guess),test$Survived)

F_meas(test$Survived,as.factor(female_guess))
F_meas(test$Survived,as.factor(class_guess))
F_meas(test$Survived,as.factor(female1_guess))
