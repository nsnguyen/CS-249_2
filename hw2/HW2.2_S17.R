
# coding: utf-8

# # HW2 Problem #2:  LDA and QDA
# 
# ## Due Sunday April 30, at 11:55pm

# ### Problem:  given a tabular dataset of feature values $X$ and classes $\boldsymbol{y}$, derive both LDA and QDA models, and determine how accurate they are.
# 
# More specifically, develop a program that reads in a numeric table with X and y from stdin, determines the normal distribution parameters for X, and derive the LDA and QDA models described in Chapter 4.4 of the ISL text (an introduction), and Chapter 4.3 of the ESL text (more rigorous treatment).
# 
# The columns of $X$ should all be numeric.  The values in the last column, $\boldsymbol{y}$,
# can be either symbolic or numeric.
# For example, with the <tt>iris</tt> dataset, the last column is symbolic.
# 
# Your program should print the <i>confusion matrix</i> for LDA,
# and also print the confusion matrix for QDA.
# 
# If there are $k$ different classes, the confusion matrix is a $k \times k$ table
# whose $i,j$-th entry is the number of times that an input row $\boldsymbol{x}$
# was classified (by LDA or QDA) as being in class $i$, when in fact its $y$ value is $j$.
# 
# Your program should print the confusion matrices both for LDA and for QDA.
# For example, with the iris dataset (as described below) the confusion matrices are 3x3, and your output should look like this:
# 
# <pre>
# 50 0 0
# 0 48 1
# 0 2 49
# 50 0 0
# 0 48 1
# 0 2 49
# </pre>
# 
# There are implementations of LDA and QDA in the MASS package,
# and you are permitted to use them.
# However, as the texts show, it is not difficult to implement either LDA or QDA.
# 
# 
# 

# ##  Centroids of the 3 classes in the iris dataset
# 
# The $X$ matrix is of size $150 \times 4$, with 4 features
# (Sepal Length, Sepal Width, Petal Length, Petal Width). The $150 \times 1$ vector $\boldsymbol{y}$ gives classes for these irises.
# Each of the 3 classes has 50 observations.
# 
# We can compute the centroids of each class (means of each gaussian) using R.
# 

# In[18]:

data(iris)

X = data.matrix(iris[,1:4]) #  equivalently:  X = data.matrix(iris[, -5 ])
y = unclass(iris[,5])       #  equivalently:  y = unclass(iris$Species)

# print summary statistics for each kind of iris

print( by( X, y, summary ) )  # summarizes the subsets of X "grouped by" y

# the Mean values in these summaries give the centroids of each cluster/MVN


# In[19]:

# printing only the mean value/centroid for each class

classes = as.character( unique(iris$Species) )
k = length(classes)

for (j in 1:k) {
    cat( classes[j], "\n" )
    Xc = subset( X, (y == j) )
    print( apply(Xc, 2, mean) ) # vector of means for each column
}


# ##  Sample use of LDA and QDA -- with the iris dataset
# 
# The $X$ matrix is of size $150 \times 4$, with 4 features
# (Sepal Length, Sepal Width, Petal Length, Petal Width).
# Thus each column in this dataset is a random sample from a different distribution,
# but they are nontrivially correlated.
# 
# The $150 \times 1$ vector $\boldsymbol{y}$ gives classes for these irises.
# Each of the 3 classes has 50 observations.
# 

# In[20]:

# load the MASS package, which includes simple implementations of LDA and QDA

not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])

if (not.installed("MASS"))  install.packages("MASS")  # we need the MASS package

library(MASS)  #  load the MASS package

#  ?lda      #  help for the LDA classifier
#  ?qda      #  help for the QDA classifier


# ## run LDA on the iris data

# In[21]:

X = data.matrix(iris[,1:4]) #  equivalently:  X = data.matrix(iris[, -5 ])
y = unclass(iris[,5])       #  equivalently:  y = unclass(iris$Species)

LDA.model <- lda(y ~ X)

## equivalently:
#  LDA.model <- qda(Species ~ ., iris)

cat("\n\nCentroids of the model:\n\n")
print( LDA.model$means )

cat("\n\nScaling of the model:\n\n")
print( LDA.model$scaling )

cat("\n\nWhat the model data structure looks like:\n\n")
str(LDA.model)   #  str() lets us inspect all the information in the model data structure


# ## In R, the function predict() is used to turn a model into a function
# 
# If $M$ is a model, and $X$ is a (possibly new) set of $X$ values, then  <b>predict(M, X)</b> yields the vector of $\boldsymbol{y}$ values predicted by the model for the input matrix $X$.
# 
# If $M$ is a classification model, then <b>predict(M, X)</b> yields the classifications for feature vectors in the rows of $X$.

# In[22]:

LDAclassifier = function(Model,X)  {
   predict(Model, as.data.frame(X))$class
}

# Compute the LDA predictions ("classifications") for each input row.

LDA.classifications = LDAclassifier(LDA.model, X)

# Find all points whose classifications didn't agree with LDA.

LDA.disagreements = (1:nrow(X))[ LDA.classifications != y ]
print(LDA.disagreements)  # print row numbers where LDA differed from y


# In[23]:

# Tabulate the number of LDA classification values vs. y values

LDA.confusion.matrix = table( LDA.classifications, y )

rownames( LDA.confusion.matrix ) = classes
colnames( LDA.confusion.matrix ) = classes
print( LDA.confusion.matrix )


# ## Print the confusion matrix in the format required

# In[24]:

print_matrix = function(Matrix) {
    for (i in 1:nrow(Matrix)) {
       cat( Matrix[i,], "\n" )  # print each row as a sequence
    }
}


LDA.confusion.matrix = table( LDA.classifications, y )

print_matrix( LDA.confusion.matrix )


# ## run QDA on the iris data

# In[25]:

QDA.model <- qda(y ~ X)

## equivalently:
#  QDA.model <- qda(Species ~ ., iris)

cat("\n\nCentroids of the model:\n\n")
print( QDA.model$means )

cat("\n\nScaling of the model:\n\n")
print( QDA.model$scaling )

cat("\n\nWhat the model object looks like:\n\n")
str(QDA.model)   #  str() lets us inspect all the information in the model data structure

QDAclassification = function(Model,X)  {
   predict(Model,as.data.frame(X))$class
}

# find all points whose classifications didn't agree with QDA

cat("\n\nNumbers of misclassified instances with the Quadratic Discriminant:\n\n")

QDA.disagreements = (1:nrow(X))[ QDAclassification(QDA.model, X) != y ]
cat(QDA.disagreements, "\n\n")

cat("\n\nData for the instances misclassified with the Quadratic Discriminant:\n\n")

print(iris[QDA.disagreements,])


# ## That's it!  Now write an R function that does this for any table it is given.
# 
# Write a function in R called <tt>LDA_and_QDA(Table)</tt>
# that takes an R data.frame called <tt>Table</tt>
# and then prints the confusion matrices for both LDA and for QDA.
# 
# To get you started, your R function can be an extension of this outline below:
# 
# <pre>
# LDA_and_QDA = function(Table) {
# 
#     X = data.matrix( Table[, 1:(ncol(Table)-1) ]
#     classifications = Table[, ncol(Table) ]
# 
#     y = unclass(classifications)  # convert the class values into numeric indices
# 
#     n = nrow(X)
#     p = ncol(X)
# 
#     # ... construct an LDA representation of X
#     # ... determine for which rows in X the LDA classification differs from y
#     # ... print the confusion matrix for LDA
# 
#     # ... construct a QDA representation of X
#     # ... determine for which rows in X the QDA classification differs from y
#     # ... print the confusion matrix for QDA
#     
# }
# </pre>
# 
# Put your function in a file called <tt>LDA_and_QDA.R</tt>, and upload it to CCLE.

# # What your program's output should look like
# 
# For example, with the iris dataset, the confusion matrices are $3 \times 3$, and it turns out the LDA and QDA matrices are identical.
# Thus the output should look like this:
# 
# <pre>
# 50 0 0
# 0 48 1
# 0 2 49
# 50 0 0
# 0 48 1
# 0 2 49
# </pre>

# #  Before submitting it, test your function on two datasets:
# 
# ### 1. Try your function on the "vowels" dataset.
# 
# <pre>
# vowels = read.csv("vowels.csv", header=TRUE)  # vowels.csv is included with this assignment
# 
# LDA_and_QDA(vowels)
# 
# ##  'y', the last column, is an integer between 1 and 11 (the classification of the vowel sound).
# </pre>
# 
# ### 2. Try your function on the "spam" dataset.
# 
# <pre>
# spam = read.csv("spam.csv", header=TRUE)  # spam.csv is included with this assignment
# 
# LDA_and_QDA(spam)
# 
# ##  'is_spam', the last column, is 2 if the classification is spam, 1 if not spam.
# </pre>
# 
# You can include your output for these two tests in your notebook here,
# or in some separate file.
# Submit this output along with the rest of your HW2 materials.
# 
# ## Then:  Upload to CCLE both your R function file <tt>LDA_and_QDA.R</tt> and its output for these two test datasets.

# In[ ]:



