
# CS249 -- Spring 2017 -- D.S. Parker (c) 2017

# # Some first steps with R, using key datatypes for data science

# In[187]:

#  set the dimensions of plots for this notebook, in inches
options( repr.plot.width=5, repr.plot.height=5 )


# # A Challenge:
# 
# Notice that, when the "Auto" dataset is displayed with a pairs plot,
# all plots involving MPG are "curved", rather than linear.
# The curves appear to reflect a nonlinear relationship between the other variables and MPG.
#
# Find a function <i>f</i> such that,
# when MPG is replaced by <i>f</i>(MPG), the curves are replaced by plots that look linear.
#
# Color each point with a color reflecting its MPG value
# (such as red for low MPG, blue for medium, and green for high MPG).
#
# If you can do this, you are ready to move into the next lecture, and more advanced things.
# You can skip this lecture.
#
# Hint:  MPG is a measure of gasoline savings.
# Displacement, Weight, and Horsepower are measures of gasoline use.

# In[211]:

data(Auto)

pairs( Auto[,c(1,3,4,5,6),],
       main="Goal: find a function f that, when mpg is replaced by f(mpg), makes the curves in the first row and column look linear",
       pch=20, cex=0.25, cex.main=0.5, col="red",)


#   

# ## The Big Picture
# ### What is R good for?

# In[92]:

# R is great for exploring data -- with its central data structure:  the data frame

library(ISLR)
data(Auto)
head(Auto)


# In[93]:

#  One reason for R's success is that it permits powerful interactive data display

hist( Auto$cylinders, col="orange", breaks=seq(0.5,8.5,by=1))


# In[94]:

# Data manipulation, like slicing and dicing, is easy when you get used to it:

subset( Auto, cylinders==5 )


# In[95]:

hist( subset( Auto, cylinders==8 )$weight, col="cyan", main="weights of 8-cylinder cars" )


# In[116]:

### R has a wonderful system for obtaining and installing packages over the web

if ("ISLR" %in% installed.packages()) print("yay -- we have ISLR installed")


# In[118]:

###  R also has developed a powerful paradigm for building models
###  Trevor Hastie, one of the authors of ESL and ISL, helped develop this in the 1990s.

my_model = lm( mpg ~ weight, data = Auto )   # construct a linear model through the data

plot( Auto$weight, Auto$mpg, pch=20, col="blue")  # plot the data

abline( my_model, col="red", lwd=2)  # add the model just created


# ### An inspired work that led to the development of R
# 
# The groundbreaking 1977 book by the eminent statistician
# <a href="https://en.wikipedia.org/wiki/John_Tukey">John Tukey</a>
# titled
# <a href="https://en.wikipedia.org/wiki/Exploratory_data_analysis">Exploratory Data Analysis</a>
# advocated development of intuition about data through information display.
# 

# ### What is R not so good for?

# In[119]:

# The syntax of the language can be surreal, and semantics too

x1 <- 1:3
x2 = seq(1,3,by=1)
assign("x3", c(1,2,3))
x4 = (1:100)[1:20][1:4][1:3]
x5 = cumsum(rep(0,3) + 1)

cbind(x1, x2, x3, x4, x5)
#  bind x1, x2, x3, x4 together as columns in a table


# In[49]:

# R is an interpreted language, and often -- without some ingenuity -- therefore slow.
# On large datasets, it is often a good idea to work on a sample rather than the whole thing.

my_random_sample = sample(1:nrow(Auto), 100)  # pick a random subset of 100 rows of the data
my_random_sample

summary( Auto[my_random_sample, 1:5] )


# In[100]:

# R can be made into a heavy-duty engineering tool, but wasn't designed for that.
#  In many situations standard functions break;  they weren't intended to handle errors.

x = c(1,1,1)
cor( x, x )   #  correlation matrix


# ## Objects:  Sequences, Matrices, and Data Frames

# ### Sequences

# In[56]:

c(1,2,3)   #  how did they come up with "c" ???   concatenation

1:3

seq(1,3, by=1)

seq(1,3, length.out=3)

cumsum( rep(1,3) )  #  rep( value, n )   generates a sequence of n copies of value


# In[57]:

x = 1:3

length(x)

x[1]

x[1:2]  #  slice containing subscripts 1 and 2

x[2:3]  #  slice containing subscripts 2 and 3

x[]


# In[59]:

x = 1:3
x

y = c(x, x)
y

z = c(x, y, y)
z


# In[62]:

x = 1:3

cbind( x, x )

rbind( x, x )

rbind( rbind(x,x), rbind(x,x) )

cbind( rbind(x,x), rbind(x,x) )


# ### Matrices

# In[64]:

matrix( 0, nr=2, nc=3 )

matrix( 0, 2, 3 )

t( matrix( 0, 2, 3 ) )  # matrix transpose


# In[68]:

dimensions = dim( matrix(0, nr=2, nc=3) )

dimensions

dimensions[1]
dimensions[2]


# In[65]:

matrix( 1:6, nr=2, nc=3 )

matrix( 1:6, nr=2, nc=3, byrow=TRUE )  # insert elements 1:6 in row-major order

t( matrix( 1:6, nr=3, nc=2 ) )  #  using transpose to implement row-major order


# In[67]:


A = matrix( 1:6,  nr=2, nc=3 )
B = matrix( 7:12, nr=3, nc=2 )

A + t(B)  #  matrix sum

A %*% B   #  matrix product

det( A %*% B )  #  determinant

solve( A %*% B )  #  matrix inverse


# ## Data Frames

# In[85]:

data(Auto)

dim(Auto)

colnames(Auto)

rownames(Auto)[30:35]


# In[87]:

Auto[ 1:5, 1:3 ]   #  data frames can be treated like arrays


# In[78]:

str(Auto)  #  inspect the "structure" of the data frame


# In[79]:

#  Add a column to the Auto data.frame:

Auto$inefficiency = 1 / Auto$mpg

colnames(Auto)


# In[88]:

# constructing a data frame from scratch

My_Data_Frame = data.frame( mpg = Auto$mpg, gpm = 1/Auto$mpg, cyl = Auto$cylinders )

dim(My_Data_Frame)


# ### A few other quirky but useful things

# 
# 

# In[19]:

get_ipython().magic('pinfo attach')


# In[90]:

attach(Auto)  #  include the "namespace" of the Auto table into the R workspace

length(mpg)   #  now "mpg" is a variable in our workspace

unique(mpg)   #  show all distinct values in the mpg column


# In[91]:

detach(Auto)  #  okay, that was fun; now remove the namespace


# In[ ]:

#  Useful:
#  write.table(  Auto, file = "myfile.csv", sep=",", echo=FALSE, row.names=FALSE )  # write out a .csv file


# In[105]:

apply( Auto[, 1:4], 2, mean )  # find the means of the first 4 columns of Auto


# In[106]:

apply( Auto[, 1:4], 2, sd )   # compute standard deviations of the first 4 columns of Auto


# In[109]:

apply( Auto[,1:4], 2, var )   # compute the variance of each of the first 4 columns of Auto


# In[112]:

max( is.na(Auto[,1:4] ) )  # determine if any element in the first 4 columns of Auto is NA


# In[115]:

sum( Auto[,1:4] == 4 )   # determine how many times the value 4 occurs in the first 4 columns of Auto


#  

# # A Solution to the "Auto pairs plot" Challenge
# 
# A way of displaying columns 1,3,4,5,6 of the Auto data that replaces the curved plots by ones that appear linear.

# In[180]:

color_intervals = cut( Auto$mpg, breaks=3 )   #  cut the range of MPG values into 3 intervals
color_codes = unclass(color_intervals)        #  turn the symbolic interval names into integers
colors = c("red","green","blue")[color_codes] #  use the integers as subscripts for RGB colors

colors[1:10]


# In[181]:

#  set the dimensions of plots for this notebook, in inches
options( repr.plot.width=6, repr.plot.height=6 )


# In[202]:

f = function(x) 1/x      #  using f(x) = 1/x  makes the Auto dataset look linear!

TransformedAuto = Auto[, c(1,3:6)]
TransformedAuto$mpg = f(Auto$mpg)

pairs( TransformedAuto, col=colors, pch=20, cex=0.25 )


