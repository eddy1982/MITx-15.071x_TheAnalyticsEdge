
# load csv file
newstrain = read.csv("kaggle/NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
newstest = read.csv("kaggle/NYTimesBlogTest.csv", stringsAsFactors=FALSE)

library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
set.seed(123)

newstrain$Popular<-as.factor(newstrain$Popular)
newstrain$NewsDesk<-as.factor(newstrain$NewsDesk)
newstrain$SectionName<-as.factor(newstrain$SectionName)
newstrain$SubsectionName<-as.factor(newstrain$SubsectionName)
newstrain$NewsDesk1<-as.factor(paste(newstrain$NewsDesk,newstrain$SectionName,sep="_"))
newstrain$PubDate<-strptime(newstrain$PubDate, "%Y-%m-%d %H:%M:%S")
newstrain$Weekday = as.factor(newstrain$PubDate$wday)
newstrain$hour = as.factor(newstrain$PubDate$hour)
t1<-as.data.frame(sort(summary(newstrain$NewsDesk1)))
newstrain$var1 = as.factor(as.numeric(grepl("aig|answer|bankruptci|better|bill|child|citi|colleg|death|dinner|energi|figur|goldman|just|let|music|need|note|peopl|phone|plan|polit|put|rise|risk|scotland|seat|secur|state|step|tri|updat|way|wrong|year|affair|age|amazon|attack|billion|boy|children|civil|come|david|econom|expect|face|facebook|famili|homeland|human|less|littl|marriag|mean|miss|money|nation|one|open|report|return|rule|senat|show|warm|american|debat|democrat|exercis|global|look|necessari|presid|question|think|back|cancer|case|day|fear|help|home|job|learn|may|street|want|what|big|fight|find|kid|life|love|quandari|realli|respond|right|school|still|test|work|anoth|ferguson|isi|say|turn|voter|well|wife|word|ask|bank|live|problem|republican|take|wall|climat|doctor|parent|reader|comment|obama|talk|will|dont|appl|make|time|like|get|ebola|good|recap",newstrain$Headline,ignore.case=TRUE)))
newstrain$var2 = as.factor(as.numeric(grepl("among|benefit|better|brook|caus|collin|court|doctor|doesnt|elect|execut|isnt|lot|michael|move|polici|problem|seem|someth|three|turn|wall|well|wrong|age|almost|america|author|candid|challeng|comment|critic|expect|face|fear|feder|global|includ|job|law|learn|million|money|often|part|patrick|rais|regul|share|son|without|deal|first|former|increas|last|life|long|program|risk|student|tell|anoth|big|busi|debat|differ|discuss|diseas|made|man|mean|much|percent|place|return|right|see|bill|child|climat|countri|decis|govern|human|obama|respons|run|senat|show|whether|back|call|financi|kid|month|nation|offer|stori|street|book|case|health|polit|say|suggest|vote|care|citi|dont|ebola|exercis|give|good|news|system|thought|yearold|come|even|hard|know|plan|home|report|republican|school|tri|children|just|patient|still|appl|day|research|women|make|recent|think|unit|want|world|parent|ask|need|question|take|two|bank|becom|talk|american|find|reader|state|use|like|look|found|now|get|mani|chang|compani|time|work|year|famili|studi|presid|way|help|peopl|puzzl",newstrain$Snippet,ignore.case=TRUE)))


# newstest$Popular<-as.factor(newstest$Popular)
newstest$NewsDesk<-as.factor(newstest$NewsDesk)
newstest$SectionName<-as.factor(newstest$SectionName)
newstest$SubsectionName<-as.factor(newstest$SubsectionName)
newstest$NewsDesk1<-as.factor(paste(newstest$NewsDesk,newstest$SectionName,sep="_"))
newstest$PubDate<-strptime(newstest$PubDate, "%Y-%m-%d %H:%M:%S")
newstest$Weekday = as.factor(newstest$PubDate$wday)
newstest$hour = as.factor(newstest$PubDate$hour)
t2<-as.data.frame(sort(summary(newstest$NewsDesk1)))
newstest$var1 = as.factor(as.numeric(grepl("aig|answer|bankruptci|better|bill|child|citi|colleg|death|dinner|energi|figur|goldman|just|let|music|need|note|peopl|phone|plan|polit|put|rise|risk|scotland|seat|secur|state|step|tri|updat|way|wrong|year|affair|age|amazon|attack|billion|boy|children|civil|come|david|econom|expect|face|facebook|famili|homeland|human|less|littl|marriag|mean|miss|money|nation|one|open|report|return|rule|senat|show|warm|american|debat|democrat|exercis|global|look|necessari|presid|question|think|back|cancer|case|day|fear|help|home|job|learn|may|street|want|what|big|fight|find|kid|life|love|quandari|realli|respond|right|school|still|test|work|anoth|ferguson|isi|say|turn|voter|well|wife|word|ask|bank|live|problem|republican|take|wall|climat|doctor|parent|reader|comment|obama|talk|will|dont|appl|make|time|like|get|ebola|good|recap",newstest$Headline,ignore.case=TRUE)))
newstest$var2 = as.factor(as.numeric(grepl("among|benefit|better|brook|caus|collin|court|doctor|doesnt|elect|execut|isnt|lot|michael|move|polici|problem|seem|someth|three|turn|wall|well|wrong|age|almost|america|author|candid|challeng|comment|critic|expect|face|fear|feder|global|includ|job|law|learn|million|money|often|part|patrick|rais|regul|share|son|without|deal|first|former|increas|last|life|long|program|risk|student|tell|anoth|big|busi|debat|differ|discuss|diseas|made|man|mean|much|percent|place|return|right|see|bill|child|climat|countri|decis|govern|human|obama|respons|run|senat|show|whether|back|call|financi|kid|month|nation|offer|stori|street|book|case|health|polit|say|suggest|vote|care|citi|dont|ebola|exercis|give|good|news|system|thought|yearold|come|even|hard|know|plan|home|report|republican|school|tri|children|just|patient|still|appl|day|research|women|make|recent|think|unit|want|world|parent|ask|need|question|take|two|bank|becom|talk|american|find|reader|state|use|like|look|found|now|get|mani|chang|compani|time|work|year|famili|studi|presid|way|help|peopl|puzzl",newstest$Snippet,ignore.case=TRUE)))
                     

newstrain1<-newstrain[c(1,2,3,7,9,12,13,14)]



log = glm(Popular ~ ., data=newstrain1, family=binomial)
PredTest = predict(log, newdata=newstest, type="response")



MySubmission = data.frame(UniqueID = newstest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "kaggle/SubmissionHeadlineLog_5.csv", row.names=FALSE)



                     