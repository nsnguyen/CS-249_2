# CS249 -- Spring 2017 -- D.S. Parker (c) 2017

# # HW4 -- Diamonds

# ------------------------------------------------

# ## Assignment Overview
# 
# The goal of this assignment is for you to develop models for the <a href="http://docs.ggplot2.org/current/diamonds.html"><b>diamonds</b> dataset</a>,
# which is included in the <a href="http://ggplot2.org">ggplot2</a> package.
# 
# This is a very simple assignment:  you are asked to build four models:
# LDA or QDA, simple Linear Regression, log-scaled Linear Regression, and Logistic Regression.
# You then just upload the formulas (R commands) you used to construct these models to CCLE.
# 
# ------------------------------------------------
# 
# ### Step 0:  build the numeric.diamonds dataset
# 
# This notebook includes commands for buiding a dataset called `<code>numeric.diamonds</code>
# that you are to use for this assignment.
# 
# 
# The diamonds dataset has 3 categorical attributes (cut, color, clarity)
# that are <b>ordered</b>.
# <i>
# The <tt>numeric.diamonds</tt> dataset 
# is a numeric version of the diamonds dataset
# in which all these categorical attributes are converted to integer codes.
# </i>
# 
# For example, there are 7 colors, with the ordering: J < I < H < G < F < E < D (J is worst, D is best).
# We implement these by replacing J with the value 1,
# I with the value 2, ..., and D with the value 7.
# 
# After doing this transformation for cut and clarity also,
# the result is an entirely numeric dataset called <tt>numeric.diamonds</tt>.
# 
# In addition to this notebook, we've provided another called <tt>Diamonds.ipynb</tt> for gaining intuition
# about the data by walking through some exploratory graphics.
# Many aspects of the dataset are displayed.
# You do not have to use this notebook, it is totally optional, but it is included
# since visualization can help.
# 
# ------------------------------------------------
# 
# ### Step 1:  build a training set and test set (as subsets of numeric.diamonds) -- using your UID
# 
# First, set the random number generator seed to your UID.
# Then generate a training set and test set using the following commands:
# 
# <code>
#        MY_UID = 123456789 </code><b style="color:blue;">########## you must enter your UCLA UID here !!!</b><code>
#        set.seed( MY_UID )
# 
#        n = nrow( numeric.diamonds )
#        sample.size = 0.75 * n   ###### Use 75% of the data for the training set
#        training.row.ids = sample( (1:n), sample.size )
#        
#        my.training.set = numeric.diamonds[  training.row.ids, ]
#        my.test.set     = numeric.diamonds[ -training.row.ids, ]   # set complement of training.set.ids
# </code>
# 
# <b>
# Please use exactly these commands to construct your training set and test set.
# Also, use the training set to construct each model,
# and use the test set to compute the accuracy of each model.
# The grading program will re-compute your model and its accuracy using this method.
# </b>
# 
# ------------------------------------------------
# 
# ### Step 2: compute accuracy of 4 Baseline Models about diamonds
# 
# ------------------------------------------------
# 
# ### Step 2: build four models about diamonds
# 
# For the numeric.diamonds dataset you are to develop a notebook that builds four models in R:
# <ol><li>
# a LDA or QDA classification model that predicts a diamond's <b>Cut</b>.
# </li><li>
# a linear regression model that predicts a diamond's <b>Price</b>.
# </li><li>
# a linear regression model that predicts a diamond's <b>log10(Price)</b>.
# </li><li>
# a logistic regression model that predicts whether a diamond's <b>Price is above &dollar;1500</b>.
# </li></ol>
# 
# As an example, you might produce these models:
# <ol><li>
# <code>  qda( cut ~ price + table + color + clarity,       data=my.training.set )</code>
# </li><li>
# <code>  lm(  price ~ carat + x + y + z + clarity,         data=my.training.set )</code>
# </li><li>
# <code>  lm(  log10(price) ~ table + log10(carat) + color, data=my.training.set )</code>
# </li><li>
# <code>  glm( I(price>1500) ~ carat + table + clarity,     data=my.training.set, family = binomial )</code>
# </li></ol>
# 
# As these examples show, details matter:
# <b>you must specify the complete formula for each model in detail, listing all variables included.</b>
# 
# Please choose attributes that produce the most accuracy models you can.
# More accurate models will get a higher score; see below.
# 
# ------------------------------------------------
# 
# ### Step 3: generate a CSV file "HW4_output.csv" including your 4 models
# 
# If these were your four models, then to complete the assignment you would create
# a CSV file <tt>HW4_output.csv</tt> containing eight lines:
# 
# <code>
#       33.333, qda( cut ~ .,           data=my.training.set )
#       88.888, lm(  price ~ .,         data=my.training.set )
#       77.777, lm(  log10(price) ~ .,  data=my.training.set )
#       88.888, glm( I(price>1500) ~ ., data=my.training.set, family=binomial )
#       44.444, qda( cut ~ price + table + color + clarity, data=my.training.set )
#       99.999, lm( price ~ carat + x + y + z + clarity, data=my.training.set )
#       99.999, lm( log10(price) ~ table + log10(carat) + color, data=my.training.set )
#       99.999, glm( I(price>1500) ~ carat + table + clarity, data=my.training.set, family=binomial )
# </code>
# 
# <b>Each line gives the accuracy of a model <u>on <tt>my.test.set</tt></u></b>
# as well as <b>the exact command you used to generate the model</b>.
# There is no length restriction on the lines.
# 
# ------------------------------------------------
# 
# ### Step 4: upload your CSV file and notebook to CCLE
# 
# Finally, go to CCLE and upload:
# <ul><li>
# your output CSV file <tt>HW4_output.csv</tt>
# </li><li>
# your notebook file <tt>HW4_Diamonds.ipynb</tt>
# </li></ul>
# 
# We are not planning to run any of the uploaded notebooks.
# However, your notebook should have the commands you used in developing your models ---
# in order to show your work.
# As announced, all assignment grading in this course will be automated,
# and the notebook is needed in order to check results of the grading program.

# ##### ------------------------------------------------

# ## Rules -- building accurate models
# 
# ### All evaluation of your models' accuracy will be done using your Training and Test data.
# 
# As mentioned above, everyone will generate their own Training set and Test set
# <b style="color:blue;">after setting the random number seed to their UID</b>.
# The measure of accuracy will be determined by your Test set.
# 
# As a result, in this assignment everyone has their own accuracy objective function,
# defined by their UID.
# 
# Also: if you do not set the seed with your UID, your accuracy measures will differ from the
# ones produced by the grading program.
# 
# ### Models must be constructed using the functions lda(), qda(), lm(), and glm()
# 
# You must use the <tt>lm</tt>, <tt>glm</tt>, <tt>lda</tt> and <tt>qda</tt> functions to compute your models.
# Both <tt>lda()</tt> and <tt>qda()</tt> are in the <tt>MASS</tt> package.)
# To produce a logistic regression model, you must include the option <tt>family=binomial</tt>, or equivalently <tt>family=binomial("logit")</tt>.
# 
# However, you have complete control over which variables are used in your model,
# and over which transformations you apply to the variables.
# So many different models are possible.
# 
# ### "Accuracy" is a standard measure of performance for models
# 
# For LDA and QDA, and for logistic regression,
# the accuracy measure is the percentage of correct classifications.
# For linear regressions a standard measure is $R^2$.
# You must implement these measures as described below.
# 
# ### The grading will compare your model's accuracy against a baseline model, using your data
# 
# The grading program will use these baseline models:
# <code>
#       qda( cut ~ .,           data=my.training.set )
#       lm(  price ~ .,         data=my.training.set )
#       lm(  log10(price) ~ .,  data=my.training.set )
#       glm( I(price>1500) ~ ., data=my.training.set, family=binomial )
# </code>
# 
# For each of the four baseline models, you must do two things:
# <ol><li>
# (17 points):  compute the accuracy of the baseline model.
# </li><li>
# (8 points):  find a model that has higher accuracy than the baseline model.
# </li></ol>
# 
# It is not difficult to get 4 &ast; 17 = 68 points on this assignment.
# However, 4 models that are better than the baseline will give 4 &ast; 25 = 100 points.
# 
# 

# ------------------------------------------------

# # Generation of the numeric.diamonds Dataset
# 
# ## Use this transformed dataset for building all Models in this assignment

# In[118]:

# we need the ggplot2 package to get the "diamonds" dataset

not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])

if (not.installed("ggplot2")) install.packages("ggplot2")
library(ggplot2)
    
if (not.installed("ggbiplot")) install.packages("ggbiplot")
library(ggbiplot)


# In[119]:

data(diamonds, package="ggplot2")

summary(diamonds)


# The dataset has the following columns:
# <table>
# <tr><td>   <b>carat</b></td><td>weight of the diamond in carats, rounded to an integer  (1 carat = 0.2 grams)</td></tr>
# <tr><td>   <b>cut</b></td><td>quality of the cut  {Fair, Good, VeryGood, Premium, Ideal}</td></tr>
# <tr><td>   <b>color</b></td><td>color code: &lbrace; J &lt; I &lt; H &lt; G &lt; F &lt; E &lt; D &rbrace;  (J is worst, D is best)</td></tr>
# <tr><td>   <b>clarity</b></td><td>clarity code: &lbrace; I1 &lt; SI1 &lt; SI2 &lt; VS1 &lt; VS2 &lt; VVS1 &lt; VVS2 &lt; IF &rbrace; (I1 is worst, IF is best)</td></tr>
# 
# <tr><td>   <b>depth</b></td><td>total depth percentage  =  2*z/(x+y)</td></tr>
# <tr><td>   <b>table</b></td><td>width of top of diamond relative to widest point</td></tr>
# 
# <tr><td>   <b>price</b></td><td>in US dollars</td></tr>
# 
# <tr><td>   <b>x</b></td><td>Length in mm (numeric value between 0 and 6)</td></tr>
# <tr><td>   <b>y</b></td><td>Width  in mm (numeric value between 0 and 9)</td></tr>
# <tr><td>   <b>z</b></td><td>Depth  in mm (numeric value between 0 and 6)</td></tr>
# 
# </table>
# 
# Caution:  the datset has skewed distributions.  Please check below.
# 

# In[120]:

dim(diamonds)  # not a tiny dataset


# In[121]:

# low_prices = subset( diamonds$price, diamonds$price<5000 )

# hist( low_prices, breaks=200, col="green",
#      main = "diamond prices below $5000; notice the odd gap around 1500")


# In[122]:

# log-scaling the prices makes patterns clearer

# hist( log10(diamonds$price), breaks=50, col="skyblue",
#      main="distribution of log10(price) looks like a mixture" )

# plot(sort(log10(diamonds$price)), pch=".", col="skyblue")


# ## Basic cleaning of the data

# In[123]:

diamonds = subset( diamonds, (x>0) & (y>0) & (z>0) )  #  There are actually some zero values, we omit them.


# ### Prepare numeric encodings of "ordered categorical" values for Cut, Color, and Clarity

# In[124]:

colnames(diamonds)


# In[125]:

( colors = levels(diamonds$color) )


# In[126]:

## The levels of Colors should have the reverse ordering
##  { D > E > F > G > H > I > J }
##  (J is worst, D is best)

( levels(diamonds$color) = rev(colors) )


# In[127]:

( cuts = levels(diamonds$cut) )


# In[128]:

## The levels of Cuts have the correct ordering


# In[97]:

( clarities = levels(diamonds$clarity) )


# In[129]:

## The levels of Clarity have ordering
##  { I1 < SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF }
##  (I1 is worst, IF is best)

# ( levels(diamonds$clarity) = clarities[c(1:8)] )
#  the factor levels of 'clarity' match the real ordering.


# ### Convert the categorical values to integers -- using the unclass() function.

# In[130]:

numeric.diamonds = transform( diamonds,
                              cut = as.numeric(unclass(diamonds$cut)),
                              color = as.numeric(unclass(diamonds$color)),
                              clarity = as.numeric(unclass(diamonds$clarity))
                   )


# In[131]:

levels(diamonds$cut)
table( diamonds$cut, numeric.diamonds$cut )


# In[132]:

levels(diamonds$color)
table( diamonds$color, numeric.diamonds$color )


# In[133]:

levels(diamonds$clarity)
table( diamonds$clarity, numeric.diamonds$clarity )


# ### Inspect correlations among the Numeric Diamonds variables, as a check

# In[134]:

diamonds.correlation.matrix = cor( numeric.diamonds )

round( diamonds.correlation.matrix, 2 )


# In[135]:

# Quick PCA of a sample of the data, to see if everything looks OK

n = nrow(numeric.diamonds)
sample.size = 2000

sample.row.ids = sample( (1:n), sample.size )

numeric.diamonds.sample = numeric.diamonds[sample.row.ids, ]

diamonds.sample = diamonds[sample.row.ids, ]


# In[136]:

numeric.diamonds.pca = prcomp(numeric.diamonds.sample, scale.=TRUE)
## biplot( numeric.diamonds.pca, xlabs=rep(".",sample.size) )

# ggbiplot( numeric.diamonds.pca, var.scale = 1,
 #         groups = diamonds$cut[sample.row.ids], ellipse = TRUE ) +
 #         labs(title = "Diamonds dataset:  principal components biplot, colored by cut")
biplot(numeric.diamonds.pca, cex=0.1)

# More visualization examples are in the notebook  Diamonds.ipynb


# In[137]:

ggplot(data=diamonds, aes(x=log10(carat), y=log10(price), color=color)) + geom_smooth() +
     ggtitle( "log10(price) as a function of log10(carat), colored by diamond color")


# In[138]:

ggplot(data=diamonds, aes(x=log10(carat), y=log10(price), color=clarity)) +
     geom_smooth() +
     ggtitle( "different grades of clarity can impact accuracy of the model")


# In[139]:

### another interesting plot, suggesting impacts on linearity by cut
# ggplot(data=diamonds, aes(x=log10(carat), y=log10(price), color=cut)) + geom_smooth() +
#     ggtitle( "log10(price) as a function of log10(carat), colored by cut")


# ------------------------------------------------

# # Step 1:  generate your Training Set and Test Set from numeric.diamonds

# In[140]:

#  Please use exactly the following statements to generate these things:


set.seed( 123456789 ) ########## please enter your UCLA UID here !!!


n = nrow(numeric.diamonds)

training.sample.size = 0.75 * n  ###### Use 75% of the data for the training set

training.row.ids = sample( (1:n), training.sample.size )
       
my.training.set = numeric.diamonds[  training.row.ids, ]
my.test.set     = numeric.diamonds[ -training.row.ids, ]   # set complement of training.set.ids


# # Step 2:  compute Accuracy of the 4 Baseline Models on your Test Set

# As mentioned above, there are 4 Baseline Models:
# <code>
#       qda( cut ~ .,           data=my.training.set )
#       lm(  price ~ .,         data=my.training.set )
#       lm(  log10(price) ~ .,  data=my.training.set )
#       glm( I(price>1500) ~ ., data=my.training.set, family=binomial )
# </code>
# 
# Develop procedures to compute their accuracy:
# one for classification models
# (like lda() and qda() in the MASS package),
# one for linear regression models (like lm()),
# and one for logistic regression models (like glm(family=binomial)).
# 
# See the section <b>Measuring Accuracy of Models in this Assignment</b> below.
# 
# 
# Then: use your procedures to compute the accuracy of the Baseline Models on your Test Set.

# # Step 2:  build 4 Models improving on the Baseline Models

# ## Problem 1:  a LDA or QDA classification model that predicts a diamond's Cut.
# 
# An example of a possible model is:
# <code>
# sample_m1  =  qda( cut ~ price + table + color + clarity,       data=my.training.set )
# </code>
# 
# If this model outperforms the first Baseline Model, you are done.

# ## Problem 2:  a linear regression model that predicts Price.
# 
# An example of a possible model is:
# <code>
# sample_m2  =  lm(  price ~ carat + x + y + z + clarity,         data=my.training.set )
# </code>

# ## Problem 3: a linear regression model that predicts log10(Price).
# 
# An example of a possible model is:
# <code>
# sample_m3  =  lm(  log10(price) ~ table + log10(carat) + color, data=my.training.set )
# </code>

# ## Problem 4:  a logistic regression model that predicts whether Price is above &dollar;1500.
# 
# An example of a possible model is:
# <code>
# sample_m4  =  glm( I(price>1500) ~ carat + table + clarity,   data=my.training.set, family = binomial )
# </code>
# 
# Notice that the values of <code>I(price>1500)</code> are always 0 or 1.
# This model's predictions will be <b>0</b> if the price is below &dollar;1500,
# and <b>1</b> if the price is above &dollar;1500, so the resulting values are "binomial".
# 
# The dataset includes information on about 50 thousand diamonds.
# About 20 thousand have a price below &dollar;1500; and the others have a price above.
# 
# Thus a model that always simply predicts prices above &dollar;1500 might be right about 60% of the time.
# Your job is to do better than this baseline rate.
# 

# # Step 3: generate a CSV file "HW4_output.csv" including your model results
# 
# If these were your four models, then to complete the assignment you would create
# a CSV file <tt>HW4_output.csv</tt> containing eight lines:
# <code>
#       33.333, qda( cut ~ .,           data=my.training.set )
#       88.888, lm(  price ~ .,         data=my.training.set )
#       77.777, lm(  log10(price) ~ .,  data=my.training.set )
#       88.888, glm( I(price>1500) ~ ., data=my.training.set, family=binomial )
#       44.444, qda( cut ~ price + table + color + clarity, data=my.training.set )
#       99.999, lm( price ~ carat + x + y + z + clarity, data=my.training.set )
#       99.999, lm( log10(price) ~ table + log10(carat) + color, data=my.training.set )
#       99.999, glm( I(price>1500) ~ carat + table + clarity, data=my.training.set, family=binomial )
# </code>
# 
# Each line gives <b>the accuracy of a model <u>on <tt>my.test.set</tt></u></b>,
# and also <b>the exact command you used to generate your model</b>.
# The first four lines are for the baseline models.
# The second four lines are your improvements.
# 
# There is no length restriction on the lines; they can be as long as you want.
# These examples above are just examples,
# and they may not improve on the Baseline Models.

# # Step 4: upload your CSV file and notebook to CCLE

# Upload the files HW4_output.csv <u>and</u> your Jupyter notebook.

# ------------------------------------------------

# # Background Material
# 
# ## R Formulas, Models, and the General Linear Model:
# 
# Chapter 11 of the <a href="https://cran.r-project.org/doc/manuals/R-intro.pdf">R Introduction Manual</a>
# gives a good description of formulas, models, model updating (exploring alternative models), and the GLM.
# 
# ------------------------------------------------
# 
# ### R formulas:
# 
# Sections 11.1 in Chapter 11 of the <a href="https://cran.r-project.org/doc/manuals/R-intro.pdf">R Introduction Manual</a>
# hs a good description of formulas.  A few constructs are important for this assignment:
# <ul><li> <b>Including an Intercept</b>:
# <br/>
# <code>  y ~ x</code><br/>
# <code>  y ~ x + 1</code><br/>
# <code>  y ~ 1 + x</code><br/>
# An intercept is included by default,
# so all of these formulas correspond to the same simple linear regression model of <code>y</code> on <code>x</code>.
# <br/>
# </li><li> <b>Omitting the Intercept</b>:
# <br/>
# <code>  y ~  x - 1</code><br/>
# <code>  y ~  x + 0</code><br/>
# <code>  y ~  0 + x</code><br/>
# <code>  y ~ -1 + x</code><br/>
# These formulas are all equivalent,
# and correspond to the linear regression of <code>y</code> on <code>x</code> without an intercept.
# <br/>
# </li><li> <b>Log-transformed Variables</b>:
# <br/>
# <code>  log(y) ~ x</code><br/>
# Regression on <code>x</code> of the transformed variable <code>log(y)</code> (with an implicit intercept term).
# <br/>
# </li><li> <b>Integer Powers of a Variable</b>:
# <br/>
# <code>  y ~ x^2</code><br/>
# <code>  y ~ 1 + x + I(x^2)</code><br/>
# <code>  y ~ poly(x,2)</code><br/>
# regression of y on a quadratic polynomial of x.
# <u>The first two formulas are equivalent --
# the power <code>x^2</code> implicitly includes the powers below it.</u>
# The third formula looks very similar, but is not completely equivalent:
# it uses "orthogonal" polynomials (with no interaction between each other),
# while the first two formulas use explicit powers, and interactions between them are considered.
# <br/>
# Note that the expression  <code>I(x^2)</code> represents an "insulated" new variable
# whose values are squares of values of <code>x</code>.
# <!--
# <br/>
# </li><li> <b>Integer Powers of a Variable</b>:
# y ~ A*B
# y ~ A + B + A:B
# y ~ B %in% A
# y ~ A/B Two factor non-additive model of y on A and B. The first two specify the same
# crossed classification and the second two specify the same nested classification. In
# abstract terms all four specify the same model subspace.
# <br/>
# </li><li> <b>Integer Powers of a Variable</b>:
# y ~ (A + B + C)^2
# y ~ A*B*C - A:B:C
# Three factor experiment but with a model containing main effects and two factor
# interactions only. Both formulae specify the same model.
# <br/>
# </li><li> <b>Integer Powers of a Variable</b>:
# y ~ A * x
# y ~ A/x
# y ~ A/(1 + x) - 1
# Separate simple linear regression models of y on x within the levels of A, with
# different codings. The last form produces explicit estimates of as many different
# intercepts and slopes as there are levels in A.
# -->
# </li></ul>
# 
# ------------------------------------------------
# 
# ### Functions on R models:
# 
# Sections 11.3 in Chapter 11 of the <a href="https://cran.r-project.org/doc/manuals/R-intro.pdf">R Introduction Manual</a>
# also gives a good description of functions on models that one can use:
# <ul><li>
# <b>coef</b>(model) <br/>
# Extract the regression coefficient (matrix).
# Long form: coefficients(object).
# <br/>
# <br/>
# </li><li>
# <b>formula</b>(model) <br/>
# Extract the model formula.
# <br/>
# <br/>
# </li><li>
# <b>plot</b>(model) <br/>
# Produce four plots, showing residuals, fitted values and some diagnostics.
# <br/>
# <br/>
# </li><li>
# <b>predict</b>(model, newdata=data.frame) <br/>
# The data frame supplied must have variables specified with the same labels as the
# original. The value is a vector or matrix of predicted values corresponding to the
# determining variable values in data.frame.
# <br/>
# <br/>
# </li><li>
# <b>residuals</b>(model) <br/>
# Extract the (matrix of) residuals, weighted as appropriate.
# <br/>
# <br/>
# </li><li>
# <b>step</b>(model) <br/>
# Select a suitable model by adding or dropping terms and preserving hierarchies. The
# model with the smallest value of AIC (Akaike’s An Information Criterion) discovered
# in the stepwise search is returned.
# <br/>
# <br/>
# </li><li>
# <b>summary</b>(model) <br/>
# Print a comprehensive summary of the results of the regression analysis.
# <br/>
# <br/>
# </li><li>
# <b>vcov</b>(model) <br/>
# Returns the variance-covariance matrix of the main parameters of a fitted model
# object.
# </li></ul>
# 
# ------------------------------------------------
# 
# ### The General Linear Model:
# 
# Sections 11.6 in Chapter 11 of the <a href="https://cran.r-project.org/doc/manuals/R-intro.pdf">R Introduction Manual</a>
# includes a tutorial on the GLM, and options for the glm() function.
# 
# Relevant for this assignment:
# <ul><li>
# <code>glm(x ~ y, family = gaussian )</code>
#     is equivalent to the usual linear regression model
# <code>lm( x ~ y )</code>    
# </li><li>
# <code>glm(x ~ y, family = gaussian("log") )</code>
#     is equivalent to the log-linear model
# <code>lm( log(x) ~ y )</code>   
# </li><li>
# <code>glm(x ~ y, family = binomial )</code>
#     is the logistic regression model
# </li></ul>

# ------------------------------------------------

# # Example:  Building a Model (simple supervised learning)

# ### This example, discussed in class, uses the MASS package for supervised learning of LDA models.

# ### Construct a training set and test set (from the Iris dataset)

# In[141]:

data(iris)

n = nrow(iris)

training.sample.size = floor( 0.75 * n )  ###### Use 75% of the data for the training set

iris.ids = (1:n)

training.set = sample( iris.ids, training.sample.size ) # Generate a random sample
test.set     = iris.ids[-training.set]       # The set complement of training.set

training.set
test.set
# table(iris$Species)                  # Tabulate the number of each species
# table(iris$Species[training.set])    # Tabulate species for the training set
# table(iris$Species[test.set])        # Tabulate species for the test set


# ### Constructing an LDA model

# In[142]:

if (!(is.element("MASS", installed.packages())))  install.packages("MASS")
library(MASS)

LDA.model = lda( Species ~ ., data = iris, subset = training.set )
LDA.model


# In[143]:

str( predict( LDA.model, subset = test.set, data = iris ))


# ### Simple accuracy calculation (for a classification model)

# In[144]:

predictions = predict( LDA.model, iris[test.set, 1:4] )
print(predictions$class)
incorrect.predictions  =  (predictions$class != iris$Species[test.set] )

incorrect.ids = test.set[incorrect.predictions]

# iris[ incorrect.ids , ]

confusion.matrix = table( iris$Species[test.set], predictions$class )
confusion.matrix

accuracy = (length(test.set) - length(incorrect.ids)) / length(test.set)
accuracy


# ------------------------------------------------

# # Examples:  Measuring Accuracy of Models in this Assignment

# ## Accuracy of an LDA or QDA model is the percentage of correct classifications

# ## FILL THIS IN:

# In[145]:

# classification_accuracy = function( model, test.data, test.solutions ) {
    # .......................................................
# }


# In[146]:

# classification_accuracy( LDA.model, iris[test.set, 1:4], iris[test.set,5])

# should be about  0.97


# ## Accuracy of a Linear Regression model is its R<sup>2</sup> value

# ## FILL THIS IN:

# In[147]:

# linear_regression_accuracy = function( model, test.data, test.solutions ) {
    # .......................................................
# }


# In[148]:

sample.LR.model = lm( Petal.Length ~ Petal.Width, data = iris, subset= training.set )
# linear_regression_accuracy( sample.LR.model, iris[test.set,], iris$Petal.Length[test.set] )

# should be about  0.93


# ## Accuracy of a Logistic Regression model is the percentage of correct classifications

# ## FILL THIS IN:

# In[149]:

# logistic_regression_accuracy = function( model, test.data, test.solutions ) {
    # .......................................................
# }


# In[150]:

LRiris = transform( iris, Species = I(Species == 'Virginica') )

head(LRiris)  # For logistic regression, the Species is converted to {0, 1}


# In[151]:

sample.LR.model = suppressWarnings( glm( Species ~ ., data = LRiris,
                                        subset=training.set, family='binomial' ) )

# logistic_regression_accuracy( sample.LR.model, LRiris[test.set,], LRiris$Species[test.set] )

#  should be close to 1


# ------------------------------------------------

# In[153]:

str(summary(m2))


# In[ ]:

summary(m2)$r.squared


# In[ ]:

plot(m2, which=4)


# ## Computing Accuracy of the Baseline Models

# In[ ]:

baseline_m1 = qda( cut ~ .,             data=my.training.set )
baseline_m1
cat(sprintf("\nClassification Accuracy: %8.6f\n",
            classification_accuracy(baseline_m1, my.test.set, my.test.set$cut )))

baseline_m2 = lm(  price ~ .,           data=my.training.set )
summary(baseline_m2)
cat(sprintf("\nLinear Regression Accuracy: %8.6f\n",
            linear_regression_accuracy(baseline_m2, my.test.set, my.test.set$price )))

baseline_m3 = lm(  log10(price) ~ .,    data=my.training.set )
summary(baseline_m3)
cat(sprintf("\nLinear Regression Accuracy: %8.6f\n",
            linear_regression_accuracy(baseline_m3, my.test.set, log10(my.test.set$price) )))

baseline_m4 = glm( I(price>1500) ~ ., data=my.training.set, family=binomial )
summary(baseline_m4)
cat(sprintf("\nLogistic Regression Accuracy: %8.6f\n",
            logistic_regression_accuracy(baseline_m4, my.test.set, I(my.test.set$price>1500) )))


# ## Now: improve on the Baseline Models

# In[ ]:

#  The Baseline models:

m1 = qda( cut ~ .,           data=my.training.set )
m2 = lm(  price ~ .,         data=my.training.set )
m3 = lm(  log10(price) ~ .,  data=my.training.set )
m4 = glm( I(price>1500) ~ ., data=my.training.set, family = binomial )


# ------------------------------------------------

# # Examples: Converting between categorical and numeric variables
# 
# R uses a jargon for categorical values that some find confusing:
# <ul><li>
# a categorical variable is called a <b>factor</b>
# </li><li>
# the set of all possible factor values is called its <b>levels</b>
# </li><li>
# the levels of a factor are not the same thing as strings --- they are symbolic values.
# </li></ul>
# 
# This may seem odd at first, but it is very useful, and it is easy to get used to.

# In[ ]:

# Levels of a factor

levels(iris$Species)


# In[ ]:

#  Altering the names of levels (dangerous)

levels(iris$Species) = c( 'Setosa', 'Versicolor', 'Virginica' )
levels(iris$Species)


# In[ ]:

# Converting numeric values to levels:

cut.factor = as.factor( numeric.diamonds$cut )
levels(cut.factor)


# In[ ]:

# Converting numeric values to levels:

cut.factor.numeric = unclass( cut.factor )
# cut.factor.numeric = as.numeric( cut.factor )    ## more or less equivalent

unique( cut.factor.numeric )  # find all unique values (in a list of values)


# ------------------------------------------------

# # Examples: Transforming variables to make them more nearly Gaussian

# In[ ]:

# ?transform

transformed.diamonds  =  transform(
                                   numeric.diamonds,
                                   log10_carat = log10(carat)
                                  )

#  this example transform adds a log10_carat column 


# In[ ]:

opar = par(mfrow=c(2,1))

data = numeric.diamonds$price

hist( data, probability=TRUE, col="deepskyblue", breaks=50)
curve( dnorm(x,mean(data),sd(data)), col="red", lwd=3, add=TRUE)
abline( v=1500, col="green", lwd=3 )
mtext( "green line:  price = 1500" )
           
hist( log10(data), probability=TRUE, col="deepskyblue", breaks=50)
curve( dnorm(x,mean(log10(data)),sd(log10(data))), col="red", lwd=3, add=TRUE)
abline( v=log10(1500), col="green", lwd=3)
mtext( "green line:  price = 1500" )
  
par(opar)


# In[ ]:

ggplot( data=diamonds, aes(x=log10(price), y=log10(table), color=cut)) +
        geom_smooth() + ggtitle("This doesn't look very linear (for some cuts)")


# In[ ]:

#  Possibly of interest:
#  Box-Cox transformations are a popular way of making variables closer to Gaussian

# library(MASS)
# help(boxcox)
# example(boxcox)


# ------------------------------------------------

# ## fyi: regsubsets() might be useful for improving regression models

# In[ ]:

if (!(is.element("leaps", installed.packages())))  install.packages("leaps")
  
library(leaps)

#  regsubsets() generates regression models for subsets of sizes up to nvmax
#  regsubsets( y ~ x, data=D, nbest=3 )  #  generates the 3 best models of each size

# ?regsubsets


# ### regsubsets() searches for the best subsets of the variables

# In[ ]:

rs = regsubsets( carat ~ ., data=numeric.diamonds, nbest=1, nvmax=5 )

summary(rs)


# In[ ]:

rs.summary = summary(rs)
str(rs.summary)


# In[ ]:

cat('subset of variables included in each model:\n#var')
print(rs.summary$which * 1)


# In[ ]:

N = nrow(rs.summary$which)
for (i in 1:N) {
    cat(sprintf('\n----- model %d: ----------------------\n', i))
    print( coef(rs, i) )
    cat(sprintf(" R^2:  %7.3f\n", rs.summary$rsq[i] ))
    cat(sprintf(" BIC:    %8.4g\n", rs.summary$bic[i] ))
}


# In[ ]:

plot(rs.summary$rsq, type="l", col="blue",
     xlab="# of variables in model", ylab="R-squared of model",
     main="R^2 increases as the number of variables increases")


# In[ ]:

plot(rs.summary$bic, type="l", col="red",
     xlab="# of variables in model", ylab="BIC of model",
     main="BIC decreases as the number of variables increases")


# In[ ]:

# ?summary.regsubsets

