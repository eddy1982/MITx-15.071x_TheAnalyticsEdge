# Unit 3, The Framingham Heart Study

# Video 3

# Read in the dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy
(1069+11)/(1069+6+187+11)

# Baseline accuracy
(1069+6)/(1069+6+187+11) 

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)


# homework1
songs = read.csv("songs.csv")

table(songs$year)

mj<-subset(songs, songs$artistname=='Michael Jackson')
mj_top10<-subset(mj, mj$Top10==1)


table(songs$timesignature)


fs<-subset(songs, songs$tempo==max(songs$tempo))
songstrain<-subset(songs, songs$year<2010)
songstest<-subset(songs, songs$year=='2010')

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

songstrain = songstrain[ , !(names(songstrain) %in% nonvars) ]
songstest = songstest[ , !(names(songstest) %in% nonvars) ]

songslog1 = glm(Top10 ~ ., data=songstrain, family=binomial)
summary(songslog1)
cor(songstrain$loudness,songstrain$energy)

songslog2 = glm(Top10 ~ . - loudness, data=songstrain, family=binomial)
summary(songslog2)

songslog3 = glm(Top10 ~ . - energy, data=songstrain, family=binomial)
summary(songslog3)


# Predictions on the test set
songslog3test = predict(songslog3, type="response", newdata=songstest)

# Confusion matrix with threshold of 0.5
table(songstest$Top10, songslog3test > 0.45)
# FALSE TRUE
# 0   309    5
# 1    40   19
(309+19)/(309+5+40+19)

# table(songstest$Top10)
# You can compute the baseline accuracy by tabling 
# the outcome variable in the test set:
#   
#   table(SongsTest$Top10)
# 
# The baseline model would get 314 observations correct, and 59 wrong, 
# for an accuracy of 314/(314+59) = 0.8418231.


# homework2

parole = read.csv("parole.csv")
table(parole$violator)


parole$state<-as.factor(parole$state)
parole$crime<-as.factor(parole$crime)


# Consider a parolee who is male, of white race, aged 50 years at prison release, 
# from the state of Maryland, served 3 months, had a maximum sentence of 12 months, 
# did not commit multiple offenses, and committed a larceny. Answer the following 
# questions based on the model's predictions for this individual. (HINT: You should 
# use the coefficients of your model, the Logistic Response Function, and the Odds 
# equation to solve this problem.)
# 
# (1)According to the model, what are the odds this individual is a violator?
# (2)According to the model, what is the probability this individual is a violator?
# 
# From the logistic regression equation, we have 
# log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 
#   0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 
#   0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 
#   0.2781054*crime3 - 0.0117627*crime4. 
# This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, 
# time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, 
# crime3=0, crime4=0. We conclude that log(odds) = -1.700629.
# 
# Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted 
# probability of violation is 1/(1+exp(1.700629)) = 0.154.

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

parolelog = glm(violator ~ ., data=train, family=binomial)
summary(parolelog)
# Predictions on the test set
parolelogtest = predict(parolelog, type="response", newdata=test)
# Confusion matrix with threshold of 0.5
table(test$violator, parolelogtest > 0.5)

library(ROCR)
ROCRpred = prediction(parolelogtest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)


# homework3

loans = read.csv("loans.csv")


table(loans$not.fully.paid)


# library(mice)
# set.seed(144)
# vars.for.imputation = setdiff(names(loans), "not.fully.paid")
# imputed = complete(mice(loans[vars.for.imputation]))
# loans[vars.for.imputation] = imputed
# 
# Note that to do this imputation, we set vars.for.imputation to all variables 
# in the data frame except for not.fully.paid, to impute the values using all 
# of the other independent variables.
# Imputation predicts missing variable values for a given observation using the 
# variable values that are reported. We called the imputation on a data frame with 
# the dependent variable not.fully.paid removed, so we predicted the missing values 
# using only other independent variables.

loans = read.csv("loans_imputed.csv")
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

loanslog = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(loanslog)
# Predictions on the test set
loanstest = predict(loanslog, type="response", newdata=test)
# Confusion matrix with threshold of 0.5
table(test$not.fully.paid, loanstest > 0.5)
test$predicted.risk<-loanstest

library(ROCR)
ROCRpred = prediction(loanstest, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


max(test$predicted.risk)


loanslog1 = glm(not.fully.paid ~ int.rate, data=train, family=binomial)
summary(loanslog1)
# Predictions on the test set
loanstest1 = predict(loanslog1, type="response", newdata=test)
table(test$not.fully.paid, loanstest1 > 0.5)

ROCRpred = prediction(loanstest1, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

high<-subset(test, test$int.rate>=0.15)

# 最低往上第100位, 最低風險的100位名單
cutoff = sort(high$predicted.risk, decreasing=FALSE)[100]


selectedLoans<-subset(high, high$predicted.risk<=cutoff)

sum(selectedLoans$profit)

table(selectedLoans$not.fully.paid)


