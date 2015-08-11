# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)


# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

RunsReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsReg)



cc <- read.csv("climate_change.csv ")
str(cc)

cc_train<-subset(cc, cc$Year<2007)
cc_test<-subset(cc, cc$Year>2006)


cc_model<- lm(Temp ~ MEI+ CO2+ CH4+ N2O+ CFC.11+ CFC.12+ TSI+ Aerosols, data=cc_train)
summary(cc_model)

cc_model<- lm(Temp ~ MEI+ TSI+ Aerosols+N2O, data=cc_train)
summary(cc_model)

可以找出最佳的R square 模型
cc_model_by_step<-step(cc_model)
summary(cc_model_by_step)

# 用自己產生的model來預測test data set
# 參數要用newdata
predicttest<-predict(cc_model_by_step, newdata=cc_test)
SSE<-sum((cc_test$Temp-predicttest)^2)
SST<-sum((cc_test$Temp-mean(cc$Temp))^2)
R<-(1-(SSE/SST))

pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
# 把null都排掉
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# 可以產生很多dummy variables
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

str(pisa_t)
## tapply(Summary Variable, Group Variable, Function)
tapply(pisa_t$readingScore, pisa_t$male, mean)

summary(pisaTrain$grade)
summary(pisaTrain$male)
summary(pisaTrain$raceeth)

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)
# What is the training-set root-mean squared error (RMSE) of lmScore?
# The training-set RMSE can be computed by first computing the SSE:
#   SSE = sum(lmScore$residuals^2)
# and then dividing by the number of observations and taking the square root:
#   RMSE = sqrt(SSE / nrow(pisaTrain))
# A alternative way of getting this answer would be with the following command:
#   sqrt(mean(lmScore$residuals^2)). 

sqrt(mean((lmScore$residuals)^2))
pisaTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

SSE<-sum((pisaTest$readingScore-predTest)^2)
SST<-sum((pisaTest$readingScore-mean(pisaTest$readingScore))^2)
R_square<-(1-(SSE/SST))

sqrt(mean((lmScore$residuals)^2))


sqrt(SSE/990)

# What is the sum of squared errors of the baseline model on the testing set? 
# HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST).
SST<-sum((pisaTest$readingScore-518.0)^2)


R_square<-(1-(SSE/SST))



flutrain <- read.csv("FluTrain.csv")
flutest <- read.csv("FluTest.csv")

flutrend1<-lm(log(ILI)~Queries, data=flutrain)
predtest1 = exp(predict(flutrend1, newdata=flutest))

SSE<-sum((flutest$ILI-predtest1)^2)
SST<-sum((flutest$ILI-mean(predtest1))^2)
R_square<-(1-(SSE/SST))

# RMSE = sqrt(SSE / nrow(pisaTrain))
# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for 
# the percentage of ILI-related physician visits, on the test set?
sqrt(SSE/52)


ILILag2 = lag(zoo(flutrain$ILI), -2, na.pad=TRUE)
flutrain$ILILag2 = coredata(ILILag2)

# In these commands, the value of -2 passed to lag means to return 2 observations before 
# the current one; a positive value would have returned future observations. The parameter
# na.pad=TRUE means to add missing values for the first two weeks of our dataset, where
# we can't compute the data from 2 weeks earlier.

flutrend2<-lm(log(ILI)~Queries+ log(ILILag2), data=flutrain)
summary(flutrend2)

ILILag2 = lag(zoo(flutest$ILI), -2, na.pad=TRUE)
flutest$ILILag2 = coredata(ILILag2)

flutest$ILILag2[1] = 1.8527356
flutest$ILILag2[2] = 2.1241299

predtest2 = exp(predict(flutrend2, newdata=flutest))


SSE<-sum((flutest$ILI-predtest2)^2)
sqrt(SSE/52)

# Which model obtained the best test-set RMSE?
# The test-set RMSE of FluTrend2 is 0.294, as opposed to the 0.749 value obtained
# by the FluTrend1 model.

