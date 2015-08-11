# Unit 6 - Introduction to Clustering

# Video 6

# After following the steps in the video, load the data into R
movies = read.table("movies.csv", header=FALSE, sep="|",quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)



# Video 7

# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods
# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)
# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

# 更有效率的方式
# (1)
colMeans(subset(movies[2:20], clusterGroups == 1))
# (2)
spl = split(movies[2:20], clusterGroups) # 產生list
spl[[1]] # 看list中的內容
# (1)和(2)是一樣的功能
lapply(spl, colMeans)
temp<-as.data.frame(lapply(spl, colMeans))
temp<-round(temp[1:10],3)
# The lapply function runs the second argument (colMeans) on each element of the first argument
# (each cluster subset in spl). So instead of using 19 tapply commands, or 10 colMeans commands,
# we can output our centroids with just two commands: one to define spl, and then the lapply command.

clusterGroups = cutree(clusterMovies, k = 2)
cluster1 = subset(movies, clusterGroups==1)
cluster2 = subset(movies, clusterGroups==2)




# homework1
dailykos = read.table("dailykos.csv",header=TRUE, sep=",")
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")


plot(kosHierClust)

# Assign points to clusters
clusterGroups = cutree(kosHierClust, k = 7)
cluster1 = subset(dailykos, clusterGroups==1)
cluster2 = subset(dailykos, clusterGroups==2)
cluster3 = subset(dailykos, clusterGroups==3)
cluster4 = subset(dailykos, clusterGroups==4)
cluster5 = subset(dailykos, clusterGroups==5)
cluster6 = subset(dailykos, clusterGroups==6)
cluster7 = subset(dailykos, clusterGroups==7)

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))