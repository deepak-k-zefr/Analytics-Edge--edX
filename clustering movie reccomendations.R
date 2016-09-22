# content filtering- based on ratings by muself---clustering
# collaborative filtering- based on ratings by many paeople
# clustering=unsupervised-- dividing data into different groups, cannot predict anything.just seperate into groups.
# hierarchical clustering 
# define distance between two points
# use Euclidian distance-sqrt of distance betwween x,y.... coordinates sqaure.
#example- Toys Sory(0,1,0,0,00,1,1,1,0,) Batman-(1,1,0,,0,0,0,01,0,) where()= action, comedy, etc
# ecilidan distance=sqrt((1-0)^2+....)
# distance between clusters--sistance between closest or farthest points.
#  most common- centroid distance. all points taken into account.
# Normaize data-distance b/w points is highly influenced by scale. eg vaiables are rvenue and age

############Hierrachical clustering
# start with every point as its own cluster
# Combine two nearest clusters into one cluster based on euclidian distance
# fint Two nearest clusters each time
# in the end there is only one cluster
# pick different clusters by drawing a horizontal line through the hieararchical structure.(dendogram)
# Check if they are meaningul clusters. if they have a feature in common.
#

movies=read.table("movielens.txt",header=FALSE,sep="|", quote="\"")
str(movies)
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
head(movies)
movies$ID=NULL # remove
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL
movies=unique(movies)
str(movies)



#Hierarchical clustering
#two steps
# cal ditance
# cluster the points


distances=dist(movies[2:20],method="euclidian")
clusterMovies=hclust(distances,method="ward.D")
plot(clusterMovies)

# 3 or 4 clusters  based on dendogram
# we need more than that so look for better spot 10 clsusters
clusterGroups=cutree(clusterMovies,k=10)
# Look at each cluster
clusterGroups=cutree(clusterMovies,k=2)

tapply(movies$Action,clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)
subset(movies,Title=="Men in Black (1997)")
# 257th row in our data
clusterGroups[257]
cluster2=subset(movies,clusterGroups==2)
cluster2$Title[1:10]

# all the coloumns for cluster "1"
colMeans(subset(movies[2:20], clusterGroups == 1))
colMeans(subset(movies[2:20], clusterGroups == 2))


spl = split(movies[2:20], clusterGroups)
spl[[1]]
colMeans(spl[[1]])
lapply(spl, colMeans)
