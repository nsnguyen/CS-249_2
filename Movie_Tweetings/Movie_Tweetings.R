### Live Movie Ratings -- from Tweets
#  https://github.com/sidooms/MovieTweetings

#### Get up-to-date Movie Ratings
# 
# These ratings are updated automatically online by a process scanning current Tweets;
# see the <a href="https://github.com/sidooms/MovieTweetings">Movie Tweetings</a> project page.
# The information in the tweets has been digested into three tables -- about movies, users, and ratings.
# Up-to-date snapshots and archives are provided.

# In[1]:

rm(list=ls())

URL = "https://raw.githubusercontent.com/sidooms/MovieTweetings/master/latest/ratings.dat"

Ratings = read.table( URL, sep = ":", header=FALSE ) [,c(1,3,5,7)]
colnames(Ratings) = c("UserID", "MovieID", "Rating", "TwitterID")

head(Ratings)


###  Some analysis of ratings

# In[2]:

# Example:  Histogram of Rating values

options( repr.plot.width=4, repr.plot.height=4 ) # control plot dimensions

h = hist( Ratings$Rating, breaks = 0:10, col="cyan" )  # compute and plot the histogram


# In[3]:

barplot( h$counts, col="aquamarine", main="histogram of all recent rating values" )


# In[4]:

# Another more general way of counting:  table()

CountOfRatings = as.data.frame(table( Ratings$Rating, dnn="rating" ), responseName="count")
CountOfRatings


# In[5]:

# Summary statistics

summary( Ratings$Rating )


#### Load the fitdistr() function

# In[6]:

not.installed = function(package_name)  !is.element(package_name, installed.packages()[,1])
if (not.installed("MASS")) install.packages("MASS")

library(MASS)

# example(fitdistr)  #  see some examples of how to use the fitdistr() function


### Problem 1: Derive a distribution (pdf) for all Rating values

# In[7]:

# Find a distribution (pdf) that describes, as well as possible,
# the distribution of all Rating values


# In[8]:

hist( Ratings$Rating, probability=TRUE, border=1, col="palegreen", breaks=0:11 )

MLEparameters = fitdistr( Ratings$Rating+1, "lognormal" )

MLEparameters

curve( dlnorm(x+0.5, meanlog=MLEparameters$estimate[1], sdlog=MLEparameters$estimate[2]), col="red", add=TRUE )  # doesn't fit very well

mtext( "the lognormal distribution doesn't fit very well", side=3, col="red" )


### Problem 2: Derive a distribution (pdf) for: Ratings per User

# In[9]:

# Find a distribution (pdf) that describes, as well as possible:
#   { (the number of Ratings per UserID)  | for each User that has made 3 or more Ratings  }


### Problem 3: Derive a distribution (pdf) for: Ratings per Movie

# In[10]:

# Find a distribution (pdf) that describes, as well as possible:
#   { (the number of Ratings per MovieID) | for each Movie that has 10 or more Ratings }


### Analysis of other data
# 
#### Get the Movie name and genre information for the Ratings above

# In[13]:

URL = "https://raw.githubusercontent.com/sidooms/MovieTweetings/master/latest/movies.dat"
MovieText = readLines( URL )

Movies = matrix( sapply( MovieInformation,
                        function(x) unlist(strsplit(sub(" [(]([0-9]+)[)]", "::\\1",x),"::"))[1:4] ),
                            nrow=length(MovieInformation), ncol=4, byrow=TRUE)
colnames(Movies) = c("MovieID", "MovieTitle", "Year", "Genres")

head(Movies)


#### Analyze which Genres are covered by these movies

# In[14]:

Genres = sort( unique(unlist(strsplit(Movies[,4], "[|]"))) )
Genres


# In[15]:

GenreCounts = sort( table( c(unlist(strsplit(Movies[,4], "[|]"))) ), decreasing=TRUE )
GenreCounts


# In[16]:

# The result above is a table with row names

rownames(GenreCounts)


#### Join the Ratings and Movie tables

# In[17]:

Ratings_and_Movies = merge( Ratings, Movies, by="MovieID" )

head(Ratings_and_Movies)


# In[19]:

options( repr.plot.width=8, repr.plot.height=8 )
opar = par(mfrow=c(3,3))  #  generate a 3 x 3 array of plots

for (g in rownames(GenreCounts)[7:15]) {  # look at less-popular Genre Types
    GenreRatings = subset( Ratings_and_Movies$Rating, grepl( g, Ratings_and_Movies$Genres ) )
    h = hist(GenreRatings, breaks=0:11, main=paste("ratings for",g,"films"), col="orange")
    points(spline((0.5:10.5),h$counts), type="l", col="blue", lwd=2 )
}

par(opar)


# In[ ]:

## There is also TwitterID information about Users, but we ignore it.
# Users   = read.table( "https://raw.githubusercontent.com/sidooms/MovieTweetings/master/latest/users.dat",
#                       sep = ":", header=FALSE )[,c(1,3)]
# colnames(Users) = c("UserID", "TwitterID")

