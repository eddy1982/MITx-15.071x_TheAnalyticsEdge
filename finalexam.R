
# exam 1

air = read.csv("AirlineDelay.csv")

library(caTools)
set.seed(15071)

spl = sample(nrow(air), 0.7*nrow(air))

AirlinesTrain = air[spl,]
AirlinesTest = air[-spl,]

air_train_lm = lm(TotalDelay ~ ., data=AirlinesTrain)
summary(air_train_lm)


cor(AirlinesTrain$NumPrevFlights, AirlinesTrain$PrevFlightGap)
cor(AirlinesTrain$OriginAvgWind , AirlinesTrain$OriginWindGust)


predicttest<-predict(air_train_lm, newdata=AirlinesTest)
SSE<-sum((AirlinesTest$TotalDelay-predicttest)^2)
SST<-sum((AirlinesTest$TotalDelay-mean(AirlinesTrain$TotalDelay))^2)
R<-(1-(SSE/SST))

air$DelayClass = factor(ifelse(air$TotalDelay == 0, "No Delay", ifelse(air$TotalDelay >= 30, "Major Delay", "Minor Delay")))

air$TotalDelay = NULL
set.seed(15071)
spl = sample.split(air$DelayClass, SplitRatio = 0.7)
AirlinesTrain = subset(air, spl==TRUE)
AirlinesTest = subset(air, spl==FALSE)


library(rpart)
library(rpart.plot)

CARTb = rpart(DelayClass ~ ., data=AirlinesTrain, method="class")
prp(CARTb)


PredictTest = predict(CARTb, newdata = AirlinesTrain, type = "class")
table(AirlinesTrain$DelayClass, PredictTest)
summary(AirlinesTrain$DelayClass)
3282/(1118+2167+3282)
[1] 0.4997716

PredictTest = predict(CARTb, newdata = AirlinesTest, type = "class")
table(AirlinesTest$DelayClass, PredictTest)



# exam 2

ebay = read.csv("ebay.csv",stringsAsFactors=FALSE)


summary(ebay$sold)
summary(ebay$size)

ebay$sold<-as.factor(ebay$sold)
ebay$biddable<-as.factor(ebay$biddable)
ebay$size<-as.factor(ebay$size)
ebay$condition<-as.factor(ebay$condition)
ebay$heel<-as.factor(ebay$heel)
ebay$style<-as.factor(ebay$style)
ebay$color<-as.factor(ebay$color)
ebay$material<-as.factor(ebay$material)


set.seed(144)
library(caTools)
spl = sample.split(ebay$sold, 0.7)
train = subset(ebay, spl==TRUE)
test = subset(ebay, spl==FALSE)


library(ROCR)
library(gplots)

log<-glm(sold ~ biddable + startprice + condition + heel + style + color + material,data=train, family="binomial")
summary(log)

# Consider a shoe that is not for auction (biddable=0), that has start price $100, that is in condition "Pre-owned", that has "High" heels, that has style "Open Toe", that has color "Black", and that has material "Satin". What is the predicted probability that this shoe will be sold according to the logistic regression model?
# 
# The observation has biddable=0, startprice=100, condition="Pre-owned", heel="High", style="Open Toe", color="Black", and material="Satin". Therefore, the prediction has logistic function value 0.5990788 + 100*-0.0044423 - 0.4952981 + 0.1224260 + 0.2226547 - 1.1078098 = -1.103178. Then you need to plug this into the logistic response function to get the predicted probability.


# The coefficients of the model are the log odds associated with that variable; so we see that the odds of being sold are exp(0.8325406)=2.299153 those of an otherwise identical shoe in the baseline category for the style variable (which is "Open Toe"). This means the stiletto is predicted to have 129.9% higher odds of being sold.


PredictTest<-predict(log, newdata=test, type="response")
table(test$sold, PredictTest>0.5)

# > table(test$sold, PredictTest>0.5)
# 
# FALSE TRUE
# 0   877   22
# 1   182   58
# 
# Obtain test-set predictions with the predict function, remembering to pass type="response". Using table, you can see that there are 80 test-set predictions with probability 0.5 or greater.


library(ROCR)
ROCRpred = prediction(PredictTest, test$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


# In 10-fold cross validation, the model with each parameter setting will be trained on 10 90% subsets of the training set. Hence, a total of 30 models will be trained. The models are evaluated in each case on the last 10% of the training set (not on the testing set).
set.seed(144)
library(caret)
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.001,0.05,0.001))


library(rpart)
library(rpart.plot)
train(sold ~ biddable + startprice + condition + heel + style + color + material, data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )

CARTb = rpart(sold ~ biddable + startprice + condition + heel + style + color + material, data=train, method="class", cp=0.005 )
prp(CARTb)

PredictTest = predict(CARTb, newdata = test, type = "class")
table(test$over50k, PredictTest)


library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(ebay$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

# Remove all terms that don't appear in at least 10% of documents in the corpus
spdtm = removeSparseTerms(dtm, 0.9)
spdtm

# Convert to a data frame
descriptionText = as.data.frame(as.matrix(spdtm))

# very useful tip: sum all cols value and sort it
sort(colSums(descriptionText))

# Make all variable names R-friendly
names(descriptionText) = paste0("D", names(descriptionText))

# Copy the following variables from the eBay data frame into descriptionText:  
# 1) sold
# 2) biddable
# 3) startprice
# 4) condition
# 5) heel
# 6) style
# 7) color
# 8) material

descriptionText$sold<-ebay$sold
descriptionText$biddable<-ebay$biddable
descriptionText$startprice<-ebay$startprice
descriptionText$condition<-ebay$condition
descriptionText$heel<-ebay$heel
descriptionText$style<-ebay$style
descriptionText$material<-ebay$material

set.seed(144)
library(caTools)
spl = sample.split(ebay$sold, 0.7)
trainText = subset(descriptionText, spl==TRUE)
testText = subset(descriptionText, spl==FALSE)

glmText <-glm(sold ~ ., data=trainText, family="binomial")
summary(glmText)

library(ROCR)

PredictTest<-predict(glmText, newdata=trainText, type="response")
ROCRpred = prediction(PredictTest, trainText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)

PredictTest<-predict(glmText, newdata=testText, type="response")
ROCRpred = prediction(PredictTest, testText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)



# exam 3

HubwayTrips = read.csv("HubwayTrips.csv")

HubwayTrips$Morning<-as.factor(HubwayTrips$Morning)
HubwayTrips$Afternoon<-as.factor(HubwayTrips$Afternoon)
HubwayTrips$Evening<-as.factor(HubwayTrips$Evening)
HubwayTrips$Weekday<-as.factor(HubwayTrips$Weekday)
HubwayTrips$Male<-as.factor(HubwayTrips$Male)

temp<-subset(HubwayTrips, HubwayTrips$Weekday==1)
temp<-subset(HubwayTrips, HubwayTrips$Weekday==0)

summary(HubwayTrips)

Hubway = read.csv("HubwayTrips.csv")
library(caret)
preproc = preProcess(Hubway)
HubwayNorm = predict(preproc, Hubway)

set.seed(5000)
cluster = kmeans(HubwayNorm, centers = 10, iter.max = 1000)
str(cluster)
sort(cluster$size)
cluster$centers

set.seed(8000)
cluster = kmeans(HubwayNorm, centers = 20, iter.max = 1000)
str(cluster)
sort(cluster$size)
d<-as.data.frame(cluster$centers)











