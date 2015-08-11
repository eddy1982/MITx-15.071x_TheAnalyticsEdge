# Unit 5 - Twitter


# VIDEO 5

# Read in the data

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

str(tweets)


# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus
 
corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus

corpus[[1]]


# Convert to lower-case

corpus = tm_map(corpus, tolower)

corpus[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]

# Look at stop words 
stopwords("english")[1:200]

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]




# Video 6

# Create matrix

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)
findFreqTerms(frequencies, lowfreq=50)
findFreqTerms(frequencies, lowfreq=100)
# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)



# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)

300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)


# Logistic Regression Model
tweetLog = glm(Negative ~ ., data = trainSparse, family=binomial)
summary(tweetLog)

# Predictions on the test set
predictTest = predict(tweetLog, type="response", newdata=testSparse)

# Confusion matrix with threshold of 0.5
table(testSparse$Negative, predictTest > 0.5)


(253+33)/(253+47+22+33)

# The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline. 
# If you were to compute the accuracy on the training set instead, you would see that the
# model does really well on the training set - this is an example of over-fitting. The 
# model fits the training set really well, but does not perform well on the test set. A 
# logistic regression model with a large number of variables is particularly at risk for 
# overfitting.
# 
# Note that you might have gotten a different answer than us, because the glm function 
# struggles with this many variables. The warning messages that you might have seen in 
# this problem have to do with the number of variables, and the fact that the model is 
# overfitting to the training set. We'll discuss this in more detail in the Homework Assignment.




# homework 1

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)

wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal)

# 1) Create the corpus for the Added column, and call it "corpusAdded".
# 2) Remove the English-language stopwords.
# 3) Stem the words.
# 4) Build the DocumentTermMatrix, and call it dtmAdded.

library(tm)
library(SnowballC)

corpusAdded = Corpus(VectorSource(wiki$Added))
# Look at corpus
corpusAdded
corpusAdded[[1]]
# IMPORTANT NOTE: If you are using the latest version of the tm package, 
# you will need to run the following line before continuing (it converts 
# corpus to a Plain Text Document). This is a recent change having to do 
# with the tolower function that occurred after this video was recorded.
corpusAdded = tm_map(corpusAdded, PlainTextDocument)
# Look at stop words 
stopwords("english")[1:200]
# Remove stopwords and apple
corpusAdded = tm_map(corpusAdded, removeWords, c(stopwords("english")))
corpusAdded[[1]]
# Stem document 
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
# Remove sparse terms
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded
# Convert to a data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))



corpusRemoved = Corpus(VectorSource(wiki$Removed))
# Look at corpus
corpusRemoved
corpusRemoved[[1]]
# IMPORTANT NOTE: If you are using the latest version of the tm package, 
# you will need to run the following line before continuing (it converts 
# corpus to a Plain Text Document). This is a recent change having to do 
# with the tolower function that occurred after this video was recorded.
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)
# Look at stop words 
stopwords("english")[1:200]
# Remove stopwords and apple
corpusRemoved = tm_map(corpusRemoved, removeWords, c(stopwords("english")))
corpusRemoved[[1]]
# Stem document 
corpusRemoved = tm_map(corpusRemoved, stemDocument)
corpusRemoved[[1]]
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
# Remove sparse terms
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
# Convert to a data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))


wikiWords = cbind(wordsAdded, wordsRemoved)


# Create dependent variable
wikiWords$Vandal  = as.factor(wiki$Vandal)

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainSparse = subset(wikiWords, split==TRUE)
testSparse = subset(wikiWords, split==FALSE)

# Build a CART model to predict Vandal
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal ~ ., data=trainSparse, method="class")

prp(wikiCART)

# Evaluate the performance of the model
predictCART = predict(wikiCART, newdata=testSparse, type="class")

table(testSparse$Vandal, predictCART)

# The grepl function returns TRUE if a string is found in another string, e.g.
grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE


wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
wikiWords2$HTTP<-as.factor(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiCART2)
# Evaluate the performance of the model
predictCART = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predictCART)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiCART2)
# Evaluate the performance of the model
predictCART = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predictCART)


wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(wikiCART3)
# Evaluate the performance of the model
predictCART = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, predictCART)


# homework2 AUTOMATING REVIEWS IN MEDICINE

clinical_trial = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(clinical_trial)
summary(clinical_trial)

clinical_trial$abstract_c<-nchar(clinical_trial$abstract)
clinical_trial$title_c<-nchar(clinical_trial$title)
temp<-subset(clinical_trial, clinical_trial$abstract_c==0)
temp<-subset(clinical_trial, clinical_trial$title_c==28)

library(tm)
library(SnowballC)

corpusTitle  = Corpus(VectorSource(clinical_trial$title))
corpusAbstract  = Corpus(VectorSource(clinical_trial$abstract))


corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle[[3]]
corpusAbstract[[3]]

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))
corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))
corpusTitle[[3]]
corpusAbstract[[3]]

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
corpusTitle[[3]]
corpusAbstract[[3]]

dtmTitle= DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

sort(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))


dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial<-as.factor(clinical_trial$trial)


library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

# Build a CART model to predict Vandal
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data=train, method="class")

prp(trialCART)

predTrain = predict(trialCART)[,2]
table(train$trial, predTrain>0.5)

# Evaluate the performance of the model
predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest >= 0.5)
# 為什麼要拿掉class參數????

library(ROCR)
ROCRpred = prediction(predTest, test$trial)
as.numeric(performance(ROCRpred, "auc")@y.values)


# homework3
email = read.csv("emails.csv", stringsAsFactors=FALSE)
str(email)

table(email$spam)


email$word_count<-nchar(email$text)
temp<-subset(email, email$word_count==13)

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(email$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)



spdtm  = removeSparseTerms(dtm, 0.95)

emailsSparse  = as.data.frame(as.matrix(spdtm))

sort(colSums(emailsSparse))

emailsSparse$spam<-email$spam


temp<-subset(emailsSparse, emailsSparse$spam==0)
temp1<-as.data.frame(sort(colSums(temp)))
names(temp1)<-c('freq')
email_ham<-subset(temp1, temp1$freq>=1000)


emailsSparse$spam = as.factor(emailsSparse$spam)
colnames(emailsSparse) = make.names(colnames(emailsSparse))





library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)



# logistic Regression模型
spamLog<-glm(spam ~ ., data=train, family="binomial")
summary(spamLog)
PredictTest<-predict(spamLog, newdata=test, type="response")

temp<-as.data.frame(PredictTest)
temp1<-subset(temp, temp$PredictTest>0.99999)

table(test$spam, PredictTest>0.5)
library(ROCR)
ROCRpred = prediction(PredictTest, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)


# CART模型
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data=train, method="class")
prp(spamCART)
PredictTest = predict(spamCART, newdata = test)
table(test$spam, PredictTest[,2]>0.5)
# 用CART來計算ROC
PredictTest = predict(spamCART, newdata = test)
pred = prediction(PredictTest[,2], test$spam)
perf = performance(pred, "tpr", "fpr") 
# tpr:true postive rate
# fpr:false postive rate
plot(perf)
as.numeric(performance(pred, "auc")@y.values)


# RamdomForest模型
library(randomForest)
spamRF = randomForest(spam ~ ., data = train)
PredictForest = predict(spamRF, newdata = test, type="prob")

table(test$spam, PredictForest[,2]>0.5)
pred = prediction(PredictForest[,2], test$spam)
perf = performance(pred, "tpr", "fpr") 
# tpr:true postive rate
# fpr:false postive rate
plot(perf)
as.numeric(performance(pred, "auc")@y.values)











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
