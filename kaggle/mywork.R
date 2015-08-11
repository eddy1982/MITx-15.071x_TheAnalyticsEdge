

# load csv file
newstrain = read.csv("kaggle/NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newstest = read.csv("kaggle/NYTimesBlogTest.csv", stringsAsFactors=FALSE)

library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(ROCR)
library(tm)
set.seed(123)

newstrain$Popular<-as.factor(newstrain$Popular)
newstrain$NewsDesk<-as.factor(newstrain$NewsDesk)
newstrain$SectionName<-as.factor(newstrain$SectionName)
newstrain$SubsectionName<-as.factor(newstrain$SubsectionName)
newstrain$NewsDesk1<-as.factor(paste(newstrain$NewsDesk,newstrain$SectionName,sep=""))
newstrain$PubDate<-strptime(newstrain$PubDate, "%Y-%m-%d %H:%M:%S")
newstrain$Weekday = as.factor(newstrain$PubDate$wday)
newstrain$hour = as.factor(newstrain$PubDate$hour)
newstrain$text<-paste(newstrain$Headline,newstrain$Snippet,sep=" ")

newstrain$var1 = as.factor(as.numeric(grepl("aig|answer|bankruptci|better|bill|child|citi|colleg|death|dinner|energi|figur|goldman|just|let|music|need|note|peopl|phone|plan|polit|put|rise|risk|scotland|seat|secur|state|step|tri|updat|way|wrong|year|affair|age|amazon|attack|billion|boy|children|civil|come|david|econom|expect|face|facebook|famili|homeland|human|less|littl|marriag|mean|miss|money|nation|one|open|report|return|rule|senat|show|warm|american|debat|democrat|exercis|global|look|necessari|presid|question|think|back|cancer|case|day|fear|help|home|job|learn|may|street|want|what|big|fight|find|kid|life|love|quandari|realli|respond|right|school|still|test|work|anoth|ferguson|isi|say|turn|voter|well|wife|word|ask|bank|live|problem|republican|take|wall|climat|doctor|parent|reader|comment|obama|talk|will|dont|appl|make|time|like|get|ebola|good|recap",newstrain$Headline,ignore.case=TRUE)))

newstrain$var2<-sapply(gregexpr("\\W+", newstrain$Headline), length) + 1
newstrain$var3<-sapply(gregexpr("\\W+", newstrain$Snippet), length) + 1

split = sample.split(newstrain$Popular, SplitRatio = 0.7)
train = subset(newstrain, split==TRUE)
test = subset(newstrain, split==FALSE)






library(tm)

CorpusHeadline = Corpus(VectorSource(c(train$Headline, test$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeNumbers)
CorpusHeadline = tm_map(CorpusHeadline, stripWhitespace)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, c("york","week","new", stopwords("english")))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

CorpusHeadline[[1]]
CorpusHeadline[[2]]
CorpusHeadline[[3]]

dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99) #值越小, col越少
HeadlineWords = as.data.frame(as.matrix(sparse))

#  檢查單字出現的頻率
check_work_freq<-as.data.frame(sort(colSums(HeadlineWords)))
names(check_work_freq)<-c('freq')

rowSums(as.matrix(dtm))


colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
HeadlineWordsTrain = head(HeadlineWords, nrow(train))
HeadlineWordsTest = tail(HeadlineWords, nrow(test))

# 把所有需要的variable都copy過來
HeadlineWordsTrain$Popular = train$Popular
HeadlineWordsTest$Popular = test$Popular

HeadlineWordsTrain$WordCount = train$WordCount
HeadlineWordsTest$WordCount = test$WordCount
HeadlineWordsTrain$NewsDesk = train$NewsDesk
HeadlineWordsTest$NewsDesk = test$NewsDesk
HeadlineWordsTrain$SectionName = train$SectionName
HeadlineWordsTest$SectionName = test$SectionName
HeadlineWordsTrain$SubsectionName = train$SubsectionName
HeadlineWordsTest$SubsectionName = test$SubsectionName
HeadlineWordsTrain$WordCount = train$WordCount
HeadlineWordsTest$WordCount = test$WordCount

HeadlineWordsTrain$Weekday = train$Weekday
HeadlineWordsTest$Weekday = test$Weekday
HeadlineWordsTrain$hour = train$hour
HeadlineWordsTest$hour = test$hour

# add cluster group 
HeadlineWordsTrain$cluster<-head(myClusters,nrow(train))
HeadlineWordsTest$cluster<-tail(myClusters,nrow(test))




# Baseline accuracy 
table(HeadlineWordsTest$Popular)
# 1632/(1632+328)
# [1] 0.8326531


# logistic regression
HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)
PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")
table(HeadlineWordsTest$Popular, PredTest > 0.5)
# 基本的glm模型產出來的model正確性還好而已
# > (1543+223)/(1543+89+105+223)
# [1] 0.9010204

#Build a CART model
HeadlineWordsCART = rpart(Popular ~ ., data=HeadlineWordsTrain, method="class")
predictCART = predict(HeadlineWordsCART, newdata=HeadlineWordsTest, type="class")
table(HeadlineWordsTest$Popular, predictCART)
# > (1583+190)/(1583+49+138+190)
# [1] 0.9045918

# Random forest model
HeadlineWordsRF = randomForest(Popular ~ ., data=HeadlineWordsTrain)
predictRF = predict(HeadlineWordsRF, newdata=HeadlineWordsTest)
table(HeadlineWordsTest$Popular, predictRF>0.5)
# 目前RF模型表現最好
# > (1555+102)/(1555+77+226+102)
# [1] 0.8454082

library(ROCR)

train1<-train[c(1,2,3,7,9,12,13,15,17)]
test1<-test[c(1,2,3,7,9,12,13,15,17)]


Log = glm(Popular ~ ., data=train1, family=binomial)
PredTest = predict(Log, newdata=test1, type="response")
table(test1$Popular, PredTest > 0.5)

ROCRpred = prediction(PredTest, test1$Popular)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 超高的
# > (1547+224)/(1547+85+104+224)
# [1] 0.9035714

CART = rpart(Popular ~ ., data=train1, method="class")
predictCART = predict(CART, newdata=test1, type="class")
table(test1$Popular, predictCART)

PredictTest = predict(CART, newdata = test1)
pred = prediction(PredictTest[,2], test1$Popular)
as.numeric(performance(pred, "auc")@y.values)
# > (1579+190)/(1579+53+138+190)
# [1] 0.902551

forest = randomForest(Popular ~ ., data = train1)
PredictForest = predict(forest, newdata = test1)
table(test1$Popular, PredictForest)

PredictTest = predict(forest, newdata = test1)
pred = prediction(PredictTest, test1$Popular)
as.numeric(performance(pred, "auc")@y.values)

# > (1556+228)/(1556+76+100+228)
# [1] 0.9102041












# RamdomForest模型
# set.seed(1)
# library(randomForest)
# forest = randomForest(over50k ~ ., data = trainSmall)
# PredictForest = predict(forest, newdata = test)
# table(test$over50k, PredictForest)





happyVar = as.factor(as.numeric(grepl("happy|merry|joyful",Train$Headline,ignore.case=TRUE)
data$york = as.factor(ifelse(grepl("york",data$Headline,ignore.case=TRUE),1,0))








# Baseline accuracy 
table(test1$Popular)
(1632)/(1632+328)
# > (1632)/(1632+328)
# [1] 0.8326531


# 應用cluster analysis

HeadlineWordsTrain<-HeadlineWordsTrain[c(1:43)]
HeadlineWordsTest<-HeadlineWordsTest[c(1:43)]

Dist = dist(HeadlineWordsTrain, method="euclidean")
HierClust = hclust(Dist, method="ward.D")
plot(HierClust)

set.seed(123)
KMC = kmeans(HeadlineWords, centers = 6, iter.max = 1000)
myClusters = KMC$cluster
KMC$centers



#  找出熱門文章單字出現的頻率
popular_post<-subset(newstrain, newstrain$Popular==1)
popHeadline = Corpus(VectorSource(c(popular_post$Headline)))
popHeadline = tm_map(popHeadline, tolower)
popHeadline = tm_map(popHeadline, PlainTextDocument)
popHeadline = tm_map(popHeadline, removePunctuation)
popHeadline = tm_map(popHeadline, removeNumbers)
popHeadline = tm_map(popHeadline, stripWhitespace)
popHeadline = tm_map(popHeadline, removeWords, c("york","week","new","can", stopwords("english")))
popHeadline = tm_map(popHeadline, stemDocument)
dtm = DocumentTermMatrix(popHeadline)
sparse = removeSparseTerms(dtm, 0.995) #值越小, col越少
popWords = as.data.frame(as.matrix(sparse))

#  檢查單字出現的頻率
check_work_freq<-as.data.frame(sort(colSums(popWords)))
names(check_work_freq)<-c('freq')


findAssocs(dtm, "recap", 0.3)
findAssocs(dtm, "good", 0.3)
findAssocs(dtm, "ebola", 0.3)
findAssocs(sparse, "racism", 0.3)


#  找出熱門文章單字出現的頻率
popular_post<-subset(newstrain, newstrain$Popular==1)
popsnippet = Corpus(VectorSource(c(popular_post$text)))
popsnippet = tm_map(popsnippet, tolower)
popsnippet = tm_map(popsnippet, PlainTextDocument)
popsnippet = tm_map(popsnippet, removePunctuation)
popsnippet = tm_map(popsnippet, removeNumbers)
popsnippet = tm_map(popsnippet, stripWhitespace)
popsnippet = tm_map(popsnippet, removeWords, c("york","week","new","can","make","may","one","will","time","said","state","say", stopwords("english")))
popsnippet = tm_map(popsnippet, stemDocument)
dtm = DocumentTermMatrix(popsnippet)
sparse = removeSparseTerms(dtm, 0.99) #值越小, col越少
popWords = as.data.frame(as.matrix(sparse))
#  檢查單字出現的頻率
check_work_freq<-as.data.frame(sort(colSums(popWords)))
names(check_work_freq)<-c('freq')


