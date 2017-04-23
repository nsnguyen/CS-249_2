
# coding: utf-8

# CS249 -- Spring 2017 -- D.S. Parker &copy; 2017

# # Normal Distributions, Supervised Learning in R, LDA, and QDA
# 
# We create a random dataset with two classes:  red and blue.

# In[121]:

# create random red instances (X-values with classification y=+1)

N.red = 300
Center.red = c(2,4)
x1.red = rnorm( N.red, mean=Center.red[1], sd=1 )
x2.red = rnorm( N.red, mean=Center.red[2], sd=1 )

X.red  = data.frame( x1 = x1.red, x2 = x2.red )
y.red  = rep( +1, N.red )

avg.X.red = apply(X.red, 2, mean)   #  centroid  (2D average)  of the red points

summary(X.red)
summary(y.red)


# In[87]:

# create random blue instances (X-values with classification y=-1)

N.blue = 500
Center.blue = c(6,3)
x1.blue = rnorm( N.blue, mean=Center.blue[1], sd=1 )
x2.blue = rnorm( N.blue, mean=Center.blue[2], sd=1 )

X.blue = data.frame( x1 = x1.blue, x2 = x2.blue )
y.blue = rep( -1, N.blue )

avg.X.blue = apply(X.blue, 2, mean)   #  centroid  (2D average)  of the blue points


# In[88]:

summary(X.blue)
summary(y.blue)


# In[89]:

# construct a dataset with all points, red and blue

N = N.red + N.blue
X = as.matrix( rbind( X.red, X.blue ) )
y = c( y.red, y.blue )

avg.X = apply( X, 2, mean )


# In[90]:

summary(X)
summary(y)


# In[91]:

options( repr.plot.width=5, repr.plot.height=5 )


# In[92]:

# plot the data

xmax = 9

plot( X.red, col="red", pch=20, xlim=c(0,xmax), ylim=c(0,xmax), asp=1 )

points( X.blue, col="blue", pch=20 )

# also plot the averages for each part of the data (red, blue, and all)

points( rbind( avg.X.red, avg.X.blue, avg.X ), pch=20, cex=2, col="green" )


# # Simple Linear Classifier

# In[93]:

Dataset = data.frame( x1 = c( x1.red, x1.blue ),
                      x2 = c( x2.red, x2.blue ),
                       y = c( y.red, y.blue ) )

summary(Dataset)


# ## Create a linear classifier that uses the vector w to define a "discriminant": a boundary between two classes.
# 
# In this case w defines a "direction" vector between the two classes, and the discriminant is orthogonal to it.
# We also allow a constant c to shift the discriminant back and forth between the classes

# In[94]:

wc.Simple = solve(t(X) %*% X) %*% t(X) %*% y

#  alternatively:   wc.Simple = lsfit(X, y)$coefficients
#  alternatively:   wc.Simple = lm(y ~ X)$coefficients

c.Simple = wc.Simple[1]
w.Simple = wc.Simple[2:3]


# In[95]:

wc.Simple


# In[96]:

classifier = function(x, w, c) { 2 * ((x %*% w + c) > 0) - 1 }
                             # i.e., ((x * w + c) > 0) ? +1 : -1


# In[97]:

## See how well are simple linear discriminant works:

cat("All misclassified instances with the simple Linear discriminant:\n\n")
which( classifier(X, w.Simple, c.Simple) != y )


# # Get functions for LDA, QDA, RDA ...

# In[98]:

#  LDA and QDA are implemented in the MASS package

if (!(is.element("MASS", installed.packages())))  install.packages("MASS")

require(MASS)


# # Simple Linear Discriminant Analysis  (LDA)

# In[99]:

LDA.model = lda(y ~ ., data=Dataset )

LDA.model


# In[100]:

LDA.model$means 
#  The means (centers) of the two LDA normal densities
#  should be approximately:
#    (2,4) for class +1
#    (6,3) for class -1


# In[101]:

linear.discriminant = LDA.model$scaling  #  linear coefficients
linear.discriminant


# In[102]:

LDAclassification = function(Model,X)  {
   predict(Model, as.data.frame(X))$class
}

# find all points whose classifications didn't match

cat("All misclassified instances with the LDA discriminant:\n\n")
which( LDAclassification(LDA.model, X) != y )


# In[103]:

xmax = 9

plot( X.red, col="red", pch=20, xlim=c(0,xmax), ylim=c(0,xmax), asp=1 )

points( X.blue, col="blue", pch=20 )


points( rbind( avg.X.red, avg.X.blue, avg.X ), pch=20, cex=2, col="green" )

curve( (-(w.Simple[1]*x + c.Simple)/w.Simple[2]), col="purple", add=TRUE)  #  plot the discriminant


# # Example of how to do Supervised Learning in R

# ## LDA and QDA on the Iris dataset

# In[104]:

n = nrow(iris)

training.sample.size = 75

iris.ids = (1:n)
training.set = sample( iris.ids, training.sample.size ) # Generate a random sample
testing.set  = iris.ids[-training.set]       # The set complement of training.set


# In[105]:

training.set


# In[106]:

testing.set


# In[107]:

table(iris$Species)                  # Tabulate the number of each species


# In[108]:

table(iris$Species[training.set])    # Tabulate species for the training set


# In[109]:

table(iris$Species[testing.set])     # Tabulate species for the testing set


# In[110]:

ldaModel = lda( Species ~ ., data = iris, subset = training.set )
ldaModel


# In[111]:

lda.predictions = predict( ldaModel, iris[testing.set,] )


# In[112]:

lda.incorrect.prediction  =  (lda.predictions$class != iris$Species[testing.set])

incorrect.lda.ids =  testing.set[ lda.incorrect.prediction ]

iris[ incorrect.lda.ids , ]


# In[113]:

lda.confusion.matrix = table( iris$Species[testing.set], lda.predictions$class )
lda.confusion.matrix

lda.accuracy = (training.sample.size - length(incorrect.lda.ids)) / training.sample.size
lda.accuracy


# In[114]:

qdaModel = qda( Species ~ ., data = iris, subset = training.set )
qdaModel

qda.predictions = predict( qdaModel, iris[ testing.set ,] )


# In[115]:

qda.incorrect.prediction  =  (qda.predictions$class != iris$Species[testing.set])

incorrect.qda.ids =  testing.set[ qda.incorrect.prediction ]

iris[ incorrect.qda.ids , ]


# In[116]:

qda.confusion.matrix = table( iris$Species[testing.set], qda.predictions$class )
qda.confusion.matrix


# In[117]:

qda.accuracy = (training.sample.size - length(incorrect.qda.ids)) / training.sample.size
qda.accuracy


# # PCA vs. LDA

# In[119]:

PCA_results = prcomp(iris[,1:4], center = TRUE, scale = TRUE) 

LDA_model = lda(Species ~ ., data = iris )

projected_LDA = predict(object = LDA_model, newdata = iris)

dataset = data.frame(Species = iris$Species, PCA = PCA_results$x, LDA = projected_LDA$x)


# In[ ]:

if (!(is.element("ggplot2", installed.packages())))  install.packages("ggplot2")

library(ggplot2)


# In[126]:

ggplot(dataset) + geom_point(aes(LDA.LD1, LDA.LD2, color = Species), size = 1.5) + 
    ggtitle("LDA predictions for the Iris dataset")


# In[125]:

ggplot(dataset) + geom_point(aes(PCA.PC1, PCA.PC2, color = Species), size = 1.5) + 
    ggtitle("PCA predictions for the Iris dataset")


# In[ ]:




# In[ ]:



