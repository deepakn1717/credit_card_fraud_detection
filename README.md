# credit_card_fraud_detection
#Data is from Kaggle
# load libraries
```
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(randomForest)
```
# Importing data into RStudio
```
creditcardfraud <- read.csv("C:\\Users\\deepa\\Desktop\\Data Sets\\creditcardfraud\\creditcard.csv")
```
# understanding data
```class(creditcardfraud)
head(creditcardfraud)
str(creditcardfraud)
```
```
'data.frame':	284807 obs. of  31 variables:
 $ Time  : num  0 0 1 1 2 2 4 7 7 9 ...
 $ V1    : num  -1.36 1.192 -1.358 -0.966 -1.158 ...
 $ V2    : num  -0.0728 0.2662 -1.3402 -0.1852 0.8777 ...
 $ V3    : num  2.536 0.166 1.773 1.793 1.549 ...
 $ V4    : num  1.378 0.448 0.38 -0.863 0.403 ...
 $ V5    : num  -0.3383 0.06 -0.5032 -0.0103 -0.4072 ...
 $ V6    : num  0.4624 -0.0824 1.8005 1.2472 0.0959 ...
 $ V7    : num  0.2396 -0.0788 0.7915 0.2376 0.5929 ...
 $ V8    : num  0.0987 0.0851 0.2477 0.3774 -0.2705 ...
 $ V9    : num  0.364 -0.255 -1.515 -1.387 0.818 ...
 $ V10   : num  0.0908 -0.167 0.2076 -0.055 0.7531 ...
 $ V11   : num  -0.552 1.613 0.625 -0.226 -0.823 ...
 $ V12   : num  -0.6178 1.0652 0.0661 0.1782 0.5382 ...
 $ V13   : num  -0.991 0.489 0.717 0.508 1.346 ...
 $ V14   : num  -0.311 -0.144 -0.166 -0.288 -1.12 ...
 $ V15   : num  1.468 0.636 2.346 -0.631 0.175 ...
 $ V16   : num  -0.47 0.464 -2.89 -1.06 -0.451 ...
 $ V17   : num  0.208 -0.115 1.11 -0.684 -0.237 ...
 $ V18   : num  0.0258 -0.1834 -0.1214 1.9658 -0.0382 ...
 $ V19   : num  0.404 -0.146 -2.262 -1.233 0.803 ...
 $ V20   : num  0.2514 -0.0691 0.525 -0.208 0.4085 ...
 $ V21   : num  -0.01831 -0.22578 0.248 -0.1083 -0.00943 ...
 $ V22   : num  0.27784 -0.63867 0.77168 0.00527 0.79828 ...
 $ V23   : num  -0.11 0.101 0.909 -0.19 -0.137 ...
 $ V24   : num  0.0669 -0.3398 -0.6893 -1.1756 0.1413 ...
 $ V25   : num  0.129 0.167 -0.328 0.647 -0.206 ...
 $ V26   : num  -0.189 0.126 -0.139 -0.222 0.502 ...
 $ V27   : num  0.13356 -0.00898 -0.05535 0.06272 0.21942 ...
 $ V28   : num  -0.0211 0.0147 -0.0598 0.0615 0.2152 ...
 $ Amount: num  149.62 2.69 378.66 123.5 69.99 ...
 $ Class : int  0 0 0 0 0 0 0 0 0 0 ...
```
# converting class variable from int to factor
```
creditcardfraud$Class <- as.factor(creditcardfraud$Class)
class(creditcardfraud$Class)
```
```
[1] "factor"
```
## Predictive Modeling
# split data into test and train
```
nrow(creditcardfraud)
```
```
> nrow(creditcardfraud)
[1] 284807
```
```
set.seed(1)
split <- sample.split(creditcardfraud, SplitRatio = 0.7)
train <- subset(creditcardfraud, split == T)
test <- subset(creditcardfraud, split == F)
nrow(train) + nrow(test)
```
```
nrow(train) + nrow(test)
[1] 284807
```
```
table(test$Class)
```
```
0     1 
91721   153 
```
# logical regression model
```
glm.model <- glm(Class ~ ., data = train, family = "binomial")
glm.predict <- predict(glm.model, test, type = "response")
table(test$Class, glm.predict > 0.5)
summary(glm.model)
```
```
FALSE  TRUE
##   0 85279    16
##   1    69    79
99.900518 % accuracy using logistic regression model.
```
# decision tree
```
tree.model <- rpart(Class ~ ., data = train, method = "class")
prp(tree.model) 
fancyRpartPlot(tree.model)
summary(tree.model)
tree.predict <- predict(tree.model, test, type = "class")
confusionMatrix(test$Class, tree.predict)
```
```
confusionMatrix(test$Class, tree.predict)
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 91705    16
         1    45   108
                                          
               Accuracy : 0.9993  

```
![desion tree](https://user-images.githubusercontent.com/11747667/31587796-e2687d14-b1b5-11e7-8e3a-2262febe9ceb.JPG)
# random forest metod
```
set.seed(10)
rf.model <- randomForest(Class ~ ., data = train, ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, test)
confusionMatrix(test$Class, rf.predict)
```
```
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 91713     8
         1    45   108
                                          
               Accuracy : 0.9994          
                 95% CI : (0.9992, 0.9996)
    No Information Rate : 0.9987          
    P-Value [Acc > NIR] : 4.543e-11  

```
