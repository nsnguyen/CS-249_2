
# coding: utf-8

# # HW2 Problem #1:  Multivariate Normal Distributions
# 
# ## Due Sunday April 30, at 11:55pm

# ## Specification of the Problem:
# 
# You are given a tabular dataset containing feature values $X$ and classes $\boldsymbol{y}$,
# where the $y$ values are integers between 1 and $k$ (so $k$ is the number of classes).
# 
# (1) Derive the Gaussian/Multivariate Normal Distribution parameters for each class (the centroid/mean vector, and the covariance matrix).
# 
# (2) For each row $\boldsymbol{x}_i$ in the $X$ matrix,
# compute its Gaussian distance to each of the $k$ MVNs
# (using the Gaussian pdf / likelihood function).
# If $j_{min}$ is this closest class, and is different from $y_i$, print $i$, $j_{min}$, and $y_i$.
# 
# More specifically, develop a program that reads in a single numeric table from stdin.
# The last column is $\boldsymbol{y}$, and the columns before this define a matrix $X$.
# Your program should identify rows in the dataset that are "misclassified" by $\boldsymbol{y}$,
# and print out information about these rows.
# 
# For example, with the <b>iris</b> dataset, the $\boldsymbol{x}_i$ rows
# for $i =$ 33, 47, and 58 have this property.
# So your program should print the following:
# 
# <pre>
# 51 3 2 
# 53 3 2 
# 58 1 2 
# 60 1 2 
# 61 1 2 
# 77 3 2 
# 78 3 2 
# 80 1 2 
# 81 1 2 
# 82 1 2 
# 87 3 2 
# 94 1 2 
# 99 1 2 
# 102 2 3 
# 107 2 3 
# 114 2 3 
# 115 2 3 
# 120 2 3 
# 122 2 3 
# 124 2 3 
# 127 2 3 
# 128 2 3 
# 139 2 3 
# 143 2 3 
# 147 2 3 
# 150 2 3 
# </pre>
# 
# The three columns of this output represent the $i$, $j_{min}$, and $y_i$ values mentioned above.
# 

# ## Multivariate Normal Distributions
# 
# We can define a $p$-dimensional Gaussian function in the following way.
# Let $X$ be a $n \times p$ matrix of (normally-distributed) feature values,
# whose $p \times p$ covariance matrix is $\Sigma$,
# and whose column means are in the vector ${\boldsymbol{\mu}}$.
# 
# Assuming that $\boldsymbol{x}$ represent a $p$-dimensional value,
# then a $p$-dimensional MVN (multivariate normal distribution) is
# 
# $$
# g({\boldsymbol{x}}, {\boldsymbol{\mu}}, \Sigma) ~~=~~
# \frac{1}{{(2\,\pi)}^{p/2}} ~
# \frac{1}{\sqrt{\det\,\Sigma}} ~
# \exp\left({ \, -\frac{1}{2} \;
# {\,({\boldsymbol{x}}-{\boldsymbol{\mu}})'}
# \; \Sigma^{-1} \,
# {\,({\boldsymbol{x}}-{\boldsymbol{\mu}})}
#  \, }\right) .
# $$
# 
# Because $\Sigma$ is a covariance matrix, it is nonnegative definite (its determinant is nonnegative and the square root is defined).

# ## Example: finding the covariance matrix and 4D MVN for the iris data

# In[5]:

data(iris)

X = as.matrix(iris[, 1:4])
means  = apply(X, 2, mean)
sigmas = apply(X, 2, sd)
n = dim(X)[1]
p = dim(X)[2]

k = length(unique(iris$Species))  # number of classes

species_class_number = unclass(iris$Species)
iris_colors = c("red","green","blue")

y = species_class_number


Sigma = cov(X)  # covariance matrix
detSigma = det(Sigma)

SigmaInverse = solve(Sigma)    #  This is R's way to compute:  inverse(Sigma)

cat("\n\ncovariance matrix:\n\n")
print(Sigma)

cat("\n\ninverse covariance matrix\n\n")
print(SigmaInverse)

cat("\n\ncorrelation matrix for the Iris data:\n\n")

print( cor(iris[,1:4]) )



# In[7]:

Sigma = cov(X)  # covariance matrix
detSigma = det(Sigma)

SigmaInverse = solve(Sigma)    #  R's way to compute:  inverse(Sigma)

cat("\n\ncovariance matrix:\n\n")
print(Sigma)

cat("\n\ninverse covariance matrix\n\n")
print(SigmaInverse)

cat("\n\ncorrelation matrix for the Iris data:\n\n")

print( cor(iris[,1:4]) )

cat("\n\nthe correlation matrix for the Iris data summarizes the slopes of pairwise plots\n\n")

panel.hist = function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    hy <- h$counts; hy <- hy/max(hy)
    rect(breaks[-nB], 0, breaks[-1], hy, ...)
}

pairs(X, col=iris_colors[y], pch=16, diag.panel = panel.hist )


# ## The Multivariate Normal / Gaussian pdf as a function
# 
# $$
# g({\boldsymbol{x}}, {\boldsymbol{\mu}}, \Sigma) ~~=~~
# \frac{1}{{(2\,\pi)}^{p/2}} ~
# \frac{1}{\sqrt{\det\,\Sigma}} ~
# \exp\left({ \, -\frac{1}{2} \;
# {\,({\boldsymbol{x}}-{\boldsymbol{\mu}})'}
# \; \Sigma^{-1} \,
# {\,({\boldsymbol{x}}-{\boldsymbol{\mu}})}
#  \, }\right) .
# $$
# 

# In[8]:

g = function(xvec, meanvec, inverseCovMatrix) {
     1 / sqrt(2*pi)^2 / sqrt(detSigma) *
         exp( -1/2 * ( t(xvec-meanvec) %*% inverseCovMatrix %*% (xvec-meanvec) )[1,1] )
}


# In[9]:

# generate a surface, filled with heights of a 2D Gaussian/MVN
# using columns 1 and 2 (the Sepal Length and Sepal Width columns)

nvalues = 61
x_grid_values = seq(-3,3, length=nvalues) * sigmas[1] + means[1]
y_grid_values = seq(-3,3, length=nvalues) * sigmas[2] + means[2]

surface = matrix(0, nvalues, nvalues)
for (i in 1:nvalues) {
   for (j in 1:nvalues) {

       surface[i,j]  =  g( c(x_grid_values[i],y_grid_values[j]), means, SigmaInverse )
       
   }
}

# plot the 2D Gaussian ("bi-variate normal distribution")

persp(surface, x=x_grid_values, y=y_grid_values, main="2D normal distribution",
       xlab="Sepal Length", ylab="Sepal Width")


# ## Contour maps

# In[10]:

contour(surface, x=x_grid_values, y=y_grid_values, main="the same 2D normal distribution",
       xlab="Sepal Length", ylab="Sepal Width")

annotate_plot_with_parameters = function(x, line=0, ...) {
 mtext(sprintf("%s:   Mean = %6.3f   Min = %6.3f   Max = %6.3f   Stddev = %6.3f",
  paste(deparse(substitute(x))),
    # R uses call by name;  this incantation obtains the input expression as a string
  mean(x),min(x),max(x),sd(x)), line=line, col="blue", ...)
}

annotate_plot_with_parameters(x_grid_values,-1)
annotate_plot_with_parameters(y_grid_values,-2)

abline(v=mean(x_grid_values),col="lightgray")
abline(h=mean(y_grid_values),col="lightgray")



# In[11]:

# print summary statistics for each kind of iris

print( by( iris[,1:4], iris[,5], summary ) )

## equivalently:
# print( by( X, y, summary ) )

# notice that the Mean values in these summaries give the centroids of each cluster/MVN


# ## Print mean vectors and covariance matrices for each class

# In[12]:

for (j in 1:k) {
   Data_for_j_th_class = subset(X, (y==j) )
   mean_vector = matrix( apply(Data_for_j_th_class, 2, mean), nrow=p, ncol=1 )  # column vector
   cov_matrix = cov(Data_for_j_th_class)
   print( as.character(unique(iris$Species))[j] )
   print( mean_vector )
   print( cov_matrix )
}


# In[13]:

# load ggplot2, an important graphics/visualization package

not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])

if (not.installed("ggplot2"))  install.packages("ggplot2")  # we need the ggplot2 package

library(ggplot2)


# In[15]:

m = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))
m = m + xlim(3.5,8.5) + ylim(1.5,4.5) + geom_point() + geom_density2d()
m = m + scale_color_manual(values=c( setosa="red", versicolor="green3", virginica="blue2" ))
m = m + ggtitle("contours reflecting the actual densities of the iris data points")
print(m)


# ## That's it!  Now just write an R function <tt>MVN(Table)</tt> that finds the closest Multivariate Normal Distribution for any table (dataset).
# 
# 
# Write a function in R called <tt>MVN(Table)</tt>
# that takes a R data.frame called <tt>Table</tt>
# and then prints the means and covariance matrices for each class.
# 
# The last column in R is a classification.
# You may assume the value of the classification is an integer from 1 to k,
# where k is the number of classes.
# 
# To get you started, your R function can be an extension of this outline below:
# 
# <pre>
# MVN = function(Table) {
# 
#     X = data.matrix( Table[, 1:(ncol(Table)-1) ] )  #  data.matrix() forces data to be numeric
#     classifications = Table[, ncol(Table) ]
# 
#     k = length(unique(classifications))  #  k is the number of different classifications
#     
#     y = classifications  # the class values will always be integers from 1 to k here.
# 
#     n = nrow(X)
#     p = ncol(X)
#     
#     distance_value = matrix(0, nrow=n, ncol=k)  # matrix to record distance values
# 
#     # ... For each class j from 1 to k
#     # ...    Derive the MVN distribution parameters for the j-th class.
#     # ...    For each row x[i,] in the X matrix,
#     # ...       distance_value[i,j] = the Gaussian distance_value of x[i,] to class j 
#     # ...           (using a function like g(), defined above).
# 
#     # ... For each row x[i,] in the X matrix,
#     # ...    If jmin is the number of this closest class and is different from y[i],
#     # ...    print i, jmin, and y[i].
# 
# }
# </pre>
# 
# Put your function in a file called <tt>MVN.R</tt>, and upload it to CCLE.

# # What your program's output should look like
# 
# For example, with the <b>iris</b> dataset, the $\boldsymbol{x}_i$ rows
# for $i =$ 33, 47, and 58 have this property.
# So your program should print the following:
# 
# <pre>
# 51 3 2 
# 53 3 2 
# 58 1 2 
# 60 1 2 
# 61 1 2 
# 77 3 2 
# 78 3 2 
# 80 1 2 
# 81 1 2 
# 82 1 2 
# 87 3 2 
# 94 1 2 
# 99 1 2 
# 102 2 3 
# 107 2 3 
# 114 2 3 
# 115 2 3 
# 120 2 3 
# 122 2 3 
# 124 2 3 
# 127 2 3 
# 128 2 3 
# 139 2 3 
# 143 2 3 
# 147 2 3 
# 150 2 3 
# </pre>
# 
# The three columns of this output represent the $i$, $j_{min}$, and $y_i$ values,
# where $j_{min}$ is the number of the class to which $\boldsymbol{x}_i$ is closest.
# 

# #  Before submitting it, test your program on two datasets:
# 
# ### 1. Try your function on the "vowels" dataset.
# 
# <pre>
# ##  The vowels data is from library(ESL), but we include vowels.csv with this assignment.
# 
# vowels = read.csv("vowels.csv", header=TRUE)
# 
# MVN(vowels)
# 
# ##  'y', the last column, is an integer between 1 and 11 (the classification of the vowel sound).
# </pre>
# 
# ### 2. Try your function on the "spam" dataset.
# 
# <pre>
# ##  The spam data is from library(ESL), but we include spam.csv with this assignment.
# 
# spam = read.csv("spam.csv", header=TRUE)
# 
# MVN(spam)
# 
# ##  'is_spam', the last column, is 2 if the classification is spam, 1 if not spam.
# </pre>
# 
# You can include your output for these two tests in your notebook here,
# or in some separate file.
# Submit this output along with the rest of your HW2 materials.
# 
# ## Then:  Upload to CCLE both your R function file <tt>MVN.R</tt> and its output for these two test datasets.

# In[ ]:



