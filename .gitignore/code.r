#importing libs
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(randomForest)

#iporting data
creditcardfraud <- read.csv("C:\\Users\\deepa\\Desktop\\Data Sets\\creditcardfraud\\creditcard.csv")

#data understanding
class(creditcardfraud)
head(creditcardfraud)
str(creditcardfraud)

#convering datatype of class from int to factor
creditcardfraud$Class <- as.factor(creditcardfraud$Class)
class(creditcardfraud$Class)

#split data into test and train
nrow(creditcardfraud)
set.seed(1)
split <- sample.split(creditcardfraud, SplitRatio = 0.7)
train <- subset(creditcardfraud, split == T)
test <- subset(creditcardfraud, split == F)
nrow(train) + nrow(test)
table(test$Class)

#logical regression model
glm.model <- glm(Class ~ ., data = train, family = "binomial")
glm.predict <- predict(glm.model, test, type = "response")
table(test$Class, glm.predict > 0.5)
summary(glm.model)

#decision tree
tree.model <- rpart(Class ~ ., data = train, method = "class")
prp(tree.model) 
fancyRpartPlot(tree.model)
summary(tree.model)
tree.predict <- predict(tree.model, test, type = "class")
confusionMatrix(test$Class, tree.predict)

#random forest metod
set.seed(10)
rf.model <- randomForest(Class ~ ., data = train, ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, test)
confusionMatrix(test$Class, rf.predict)
