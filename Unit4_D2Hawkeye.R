# Unit 4 - "Keeping an Eye on Healthcare Costs" Lecture
# VIDEO 6

# Read in the data
Claims = read.csv("ClaimsData.csv")

str(Claims)

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims)

# Split the data
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl==TRUE)
ClaimsTest = subset(Claims, spl==FALSE)

# VIDEO 7

# Baseline method
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)

# Penalty Matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix

# Penalty Error of Baseline Method
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)


# According to the table output, this baseline method would get 122978 observations 
# correct, and all other observations wrong. So the accuracy of this baseline method
# is 122978/nrow(ClaimsTest) = 0.67127.
# 
# For the penalty error, since this baseline method predicts 1 for all observations, 
# it would have a penalty error of:
#   
#   (0*122978 + 2*34840 + 4*16390 + 6*7937 + 8*1057)/nrow(ClaimsTest) = 1.044301

# VIDEO 8

# Load necessary libraries
library(rpart)
library(rpart.plot)

# CART model
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)

prp(ClaimsTree)


# Make predictions
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, PredictTest)

(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest)

# Penalty Error
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

# New CART model with loss matrix
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + 
                     diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + 
                     bucket2008 + reimbursem nt2008, 
                     data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest)

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)


# homework1
gerber = read.csv("gerber.csv")

table(gerber$voting)
table(gerber$voting)/nrow(gerber)



temp<-subset(gerber, gerber$voting==1)


table(temp$hawthorne)/nrow(temp)
table(temp$civicduty)/nrow(temp)
table(temp$neighbors)/nrow(temp)
table(temp$self)/nrow(temp)



gerberlog = glm(voting ~ hawthorne + civicduty + neighbors + self, data =gerber, family=binomial)
summary(gerberlog)

# Predictions on the test set
gerbertest= predict(gerberlog, type="response", newdata=gerber)
# Confusion matrix with threshold of 0.5
table(gerber$voting, gerbertest > 0.3)
table(gerber$voting, gerbertest > 0.5)

library(ROCR)
ROCRpred = prediction(gerbertest, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)



CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)


CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)

CARTmodel5 = rpart(voting ~ control+sex, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
prp(CARTmodel5,digits = 6)

gerberlog1 = glm(voting ~ control+sex, data =gerber, family=binomial)
summary(gerberlog1)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerberlog1, newdata=Possibilities, type="response")



# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) )
# 1         2         3         4 
# 0.3462559 0.3024455 0.3337375 0.2908065 

abs(0.2908065-0.290456)
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)

# homework2

letter = read.csv("letters_ABPR.csv")
letter$isB = as.factor(letter$letter == "B")

library(caTools)

set.seed(1000)
split = sample.split(letter$isB, SplitRatio = 0.50)
# Split up the data using subset
train = subset(letter, split==TRUE)
test = subset(letter, split==FALSE)


CARTb = rpart(isB ~ . - letter, data=train, method="class")
PredictTest = predict(CARTb, newdata = test, type = "class")
table(test$isB, PredictTest)

library(randomForest)
set.seed(1000)
forest = randomForest(isB ~ . - letter, data = train)
PredictForest = predict(forest, newdata = test)
table(test$isB, PredictForest)

letter = read.csv("letters_ABPR.csv")
letter$letter = as.factor(letter$letter)
set.seed(2000)
split = sample.split(letter$letter, SplitRatio = 0.50)
# Split up the data using subset
train = subset(letter, split==TRUE)
test = subset(letter, split==FALSE)

# CART模型
CARTb = rpart(letter ~ ., data=train, method="class")
PredictTest = predict(CARTb, newdata = test, type = "class")
table(test$letter, PredictTest)

# RamdomForest模型
set.seed(1000)
forest = randomForest(letter ~ ., data = train)
PredictForest = predict(forest, newdata = test)
table(test$letter, PredictForest)


#homework3
census = read.csv("census.csv")
set.seed(2000)
library(caTools)
split = sample.split(census$over50k, SplitRatio = 0.60)
# Split up the data using subset
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

# logistic Regression模型
censuslog = glm(over50k ~ ., data=train, family="binomial")
summary(censuslog)
PredictTest<-predict(censuslog, newdata=test, type="response")
table(test$over50k, PredictTest>0.5)
library(ROCR)
ROCRpred = prediction(PredictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)


# CART模型
library(rpart)
library(rpart.plot)
CARTb = rpart(over50k ~ ., data=train, method="class")
prp(CARTb)
PredictTest = predict(CARTb, newdata = test, type = "class")
table(test$over50k, PredictTest)

# 用CART來計算ROC
PredictTest = predict(CARTb, newdata = test)
pred = prediction(PredictTest[,2], test$over50k)
perf = performance(pred, "tpr", "fpr") 
# tpr:true postive rate
# fpr:false postive rate
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

# RamdomForest模型
set.seed(1)
library(randomForest)
forest = randomForest(over50k ~ ., data = trainSmall)
PredictForest = predict(forest, newdata = test)
table(test$over50k, PredictForest)

# 因為RandomForeset比較不好解讀, 所以得用其它的方式來說明那些變因是重要的
# As we discussed in lecture, random forest models work by building a large collection of trees. 
# As a result, we lose some of the interpretability that comes with CART in terms of seeing how 
# predictions are made and which variables are important. However, we can still compute metrics 
# that give us insight into which variables are important.
# 
# One metric that we can look at is the number of times, aggregated over all of the trees in the 
# random forest model, that a certain variable is selected for a split. To view this metric, run 
# the following lines of R code (replace "MODEL" with the name of your random forest model):
  
vu = varUsed(forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))

# This code produces a chart that for each variable measures the number of times that variable was 
# selected for splitting (the value on the x-axis). Which of the following variables is the most 
# important in terms of the number of splits


# A different metric we can look at is related to "impurity", which measures how homogenous each bucket
# or leaf of the tree is. In each tree in the forest, whenever we select a variable and perform a split,
# the impurity is decreased. Therefore, one way to measure the importance of a variable is to average 
# the reduction in impurity, taken over all the times that variable is selected for splitting in all of 
# the trees in the forest. To compute this metric, run the following command in R (replace "MODEL" with
# the name of your random forest model):
  
  varImpPlot(forest)

# Which one of the following variables is the most important in terms of mean reduction in impurity?
# 
# If you generate the plot with the command varImpPlot(MODEL), you can see that occupation gives a larger
# reduction in impurity than the other variables.
# Notice that the importance as measured by the average reduction in impurity is in general different from
# the importance as measured by the number of times the variable is selected for splitting. Although age 
# and occupation are important variables in both metrics, the order of the variables is not the same in 
# the two plots.


library(caret)
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))


train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )

CARTb = rpart(over50k ~ ., data=train, method="class", cp=0.002 )
PredictTest = predict(CARTb, newdata = test, type = "class")
table(test$over50k, PredictTest)

prp(CARTb)
