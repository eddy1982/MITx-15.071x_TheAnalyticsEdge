# Unit 7 - Lecture 2, Predictive Policing


# VIDEO 3 - A Basic Line Plot

# Load our data:
mvt = read.csv("mvt.csv", stringsAsFactors=FALSE)

str(mvt)
# 日期是單純的character
# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date) #取weekday值
mvt$Hour = mvt$Date$hour         #直接取hour值
# Let's take a look at the structure of our data again:
str(mvt)
# Create a simple line plot - need the total number of crimes on each day of the week. We can get this information by creating a table:
table(mvt$Weekday)
# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)


# Load the ggplot2 library:
library(ggplot2)

# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))
# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) +
  geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")



# VIDEO 4 - Adding the Hour of the Day

# Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour)
# Save this to a data frame:
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))

str(DayHourCounts)

# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
# Create out plot:依星期幾來group
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))
# Change the colors
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=1)
# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")
# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2)
# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2, alpha=0.5)

# Fix the order of the days
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日"))

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))
# Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())




# VIDEO 5 - Maps

# Install and load two new packages:
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)
library(ggplot2)

# Load a map of Chicago into R:
# get_map(location = c(lon = -95.3632715, lat = 29.7632836),
#         zoom = "auto", scale = "auto",
#         maptype = c("terrain", "satellite", "roadmap", "hybrid", "toner", "watercolor"),
#         messaging = FALSE, urlonly = FALSE,
#         filename = "ggmapTemp", crop = TRUE,
#         color = c("color", "bw"),
#         source = c("google", "osm", "stamen", "cloudmade"),
#         api_key)

kaohsiung = get_map(location = c(lon = 120.371526, lat = 22.619793), zoom = 13, color = 'bw')
ggmap(kaohsiung)


# Look at the map
chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)

# Plot the first 100 motor vehicle thefts:
ggmap(chicago) + geom_point(data = mvt[1:300,], aes(x = Longitude, y = Latitude))
# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame for each area:
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLonCounts)
# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) +
  scale_colour_gradient(low="yellow", high="red")

# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

LatLonCounts2<-subset(LatLonCounts, LatLonCounts$Freq>0)
ggmap(chicago) + geom_tile(data = LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill="red")

# VIDEO 6 - Geographical Map on US

# Load our data:
murders = read.csv("murders.csv")
str(murders)

# Load the map of the US
kaohsiung = get_map(location = "kaohsiung", zoom = 15)

statesMap = map_data("state")
str(statesMap)
ggmap(statesMap)

# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "grey")
# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)
# Join the statesMap data and the murders data into one dataframe:
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) +
  geom_polygon(color = "grey") + scale_fill_gradient(low = "white", high = "red", guide = "legend")
# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) +
  geom_polygon(color = "grey") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000

# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) +
  geom_polygon(color = "grey") + scale_fill_gradient(low = "white", high = "red", guide = "legend")
# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) +
  geom_polygon(color = "grey") +
  scale_fill_gradient(low = "white", high = "red", guide = "legend", limits = c(0,10))


ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) +
  geom_polygon(color = "grey") +
  scale_fill_gradient(low = "white", high = "red", guide = "legend")



# homework1

library(maps)
library(ggmap)
library(ggplot2)


statesMap = map_data("state")

polling<-read.csv("PollingImputed.csv")
Train<-subset(polling, polling$Year<2009)
Test<-subset(polling, polling$Year==2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction ))+ geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")


temp<-subset(predictionDataFrame, predictionDataFrame$Test.State == 'Florida')



# homework2

edges<-read.csv("edges.csv")
users<-read.csv("users.csv")

table(edges$V1)
temp<-as.data.frame(table(edges$V1))

install.packages("igraph")
library(igraph)

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
sort(degree(g) )

V(g)$size = degree(g)/2+2
plot(g, vertex.label=V(g)$size)

sort(V(g)$size)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "B"] = "gray"
plot(g, vertex.label=V(g)$size)


?igraph.plotting



# homework3


tweets<-read.csv("tweetss.csv", stringsAsFactors=FALSE)

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
# Look at corpus
corpus
corpus[[1]]
corpus = tm_map(corpus, tolower)

# IMPORTANT NOTE: If you are using the latest version of the tm package, 
# you will need to run the following line before continuing (it converts 
# corpus to a Plain Text Document). This is a recent change having to do 
# with the tolower function that occurred after this video was recorded.
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
stopwords("english")[1:200]

# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]
frequencies = DocumentTermMatrix(corpus) 
allTweets = as.data.frame(as.matrix(frequencies))


install.packages("wordcloud")
library(wordcloud)


colnames(allTweets)
colSums(allTweets)


wordcloud(colnames(allTweets), colSums(allTweets), scale=c(4,1),max.words=Inf)                
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))
brewer.pal() 
display.brewer.all()
brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]
                
                
                
                