
# coding: utf-8

# # HW3 Problem #2: Linear Regression
# 
# ## Due Sunday May 7, at 11:55pm
# 
# ## However:  it is strongly recommended you master this material before the Midterm on Saturday May 6.  The Midterm will cover this material.

# <hr style="height: 30px; background-color: gray;"/>

# # Tutorial on Linear Regression -- adapted from [ISL] Ch.3
# 
# For more information on the steps here, read the "Lab" section in Chapter 3 of [ISL].
# 
# Chapter 3 in the [ESL] text covers more material and is more elegantly written, but is also very compact and takes time to read.

# ## Load all libraries we need

# In[22]:

not.installed = function(package_name)  !is.element(package_name, installed.packages()[,1])

if (not.installed("MASS")) install.packages("MASS")
if (not.installed("ISLR")) install.packages("ISLR")

library(MASS)
library(ISLR)


# ## Simple Linear Regression
# 
# This "lab" uses the Boston dataset in MASS, a famous dataset about housing prices in Boston.
# 
# The ISL text explains better what the variable names refer to, but of particular importance in the examples here:
# 
# <b>medv</b> = median home value
# 
# <b>lstat</b> = low socioeconomic status.
# 

# In[23]:

print(names(Boston))

print(head(Boston))


# In[24]:

lm.fit = lm(medv ~ lstat, data=Boston)

##  attach(Boston)   ## bad style
##  lm.fit=lm(medv~lstat)

print(lm.fit)

print(summary(lm.fit))

print(names(lm.fit))

print(coef(lm.fit))

print(confint(lm.fit))


# In[25]:

print( predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="confidence") )

print( predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction") )

plot(Boston$lstat, Boston$medv)

#  The following are equivalent -- they add the model to the current plot (as a line)

abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")


# In[26]:

opar = par(mfrow=c(2,2))

plot(Boston$lstat, Boston$medv, col="red")
plot(Boston$lstat, Boston$medv, pch=20)
plot(Boston$lstat, Boston$medv, pch="+")
plot(1:20,1:20,pch=1:20)

par(opar)


# In[27]:

opar = par(mfrow=c(2,2))

plot(lm.fit, col="green3", pch=16)  #  generates 4 plots

plot(predict(lm.fit), residuals(lm.fit), col="red2", pch=20)

plot(predict(lm.fit), rstudent(lm.fit), col="blue1", pch=16)

plot(hatvalues(lm.fit), col="orange3", pch=20)

par(opar)

print( which.max(hatvalues(lm.fit)) )


# ## Multiple Linear Regression
# 
# (i.e., multiple x-variables, not just one)

# In[28]:

lm.fit = lm(medv ~ lstat + age, data=Boston)
print( summary(lm.fit) )


# In[29]:

lm.fit = lm(medv ~ ., data=Boston)
print( summary(lm.fit) )


# In[30]:

lm.fit1 = lm(medv ~ . - age, data=Boston)
print( summary(lm.fit1) )


# In[31]:

lm.fit1 = update(lm.fit, ~ . - age, data=Boston)

print( summary(lm.fit1) )


# ## Interaction Terms (adding features that are not linear in the initial set of variables)

# In[32]:

print( summary(lm( medv ~ lstat * age, data=Boston)) )


# ## Nonlinear Transformations of the Predictors

# In[33]:

lm.fit2 = lm(medv ~ lstat + I(lstat^2), data=Boston )

print( summary(lm.fit2) )

lm.fit = lm(medv ~ lstat, data=Boston)
# note that lm.fit2 contains all variables in lm.fit

print( anova(lm.fit,lm.fit2) )


# In[34]:

lm.fit5 = lm(medv ~ poly(lstat,5), data=Boston)  # 5-th degree polynomial fit

print( summary(lm.fit5) )


# In[35]:

print( summary(lm(medv ~ log(rm), data=Boston)) )  # logarithmic transform


# <hr style="height: 30px; background-color: gray;"/>

# # Doing Linear Regression using only Linear Algebra
# 
# The linear regression coefficients $\boldsymbol{w}$ can be obtained directly from the least-squares solution:
# $\boldsymbol{w} ~~=~~ (X'\,X)^{-1} \; X' \; \boldsymbol{y} .$
# 
# All of the computations above can be performed with this formula.

# In[36]:

y = Boston$medv

X_ = cbind( 1, Boston$lstat )

# In the X matrix, one column is the constant 1 (for an intercept), the other is lstat.

# equivalent construction of X using R constructors and matrix assignment:

X_ = matrix(1, nrow=nrow(Boston), ncol=2)
X_[,2] = Boston$lstat

Xt_X = t(X_) %*% X_
Xt_y = t(X_) %*% y

w = solve( Xt_X, Xt_y )  #  the least squares solution for the regression coefficients

cat("\n\nregression_coefficients:\n")
print(w)

for_comparison = lm( y ~ X_ - 1 )$coefficients

cat("\n\nlm coefficients for comparison:\n")
print(for_comparison)


# ## Implementing a quadratic polynomial fit (using linear algebra)

# In[37]:

X_ = cbind( 1, Boston$lstat, Boston$lstat^2 )

# one column is the constant 1 (for an intercept), the other two are lstat and lstat^2
# equivalent construction of X using R constructors and matrix assignment:

X_ = matrix(1, nrow=nrow(Boston), ncol=3)
X_[,2] = Boston$lstat
X_[,3] = Boston$lstat^2

Xt_X = t(X_) %*% X_
Xt_y = t(X_) %*% y

w = solve( Xt_X, Xt_y )  #  the least squares solution

cat("\n\nregression_coefficients:\n")
print(w)

for_comparison = lm( y ~ X_ - 1 )$coefficients

# lm(medv ~ lstat + I(lstat^2), data=Boston )

cat("\n\nlm coefficients for comparison:\n")
print(for_comparison)


# ##With linear algebra we can even do Interaction Terms as above
# 
# --- adding features that are not linear in the initial set of variables.

# In[38]:

# the product lstat * age is an "interaction" (nonlinear function) of the variables:

X_ = cbind( 1, Boston$lstat, Boston$age, (Boston$lstat * Boston$age) )

# one column of X is the constant 1 (for an intercept), the other two are lstat and age

Xt_X = t(X_) %*% X_
Xt_y = t(X_) %*% y

w = solve( Xt_X, Xt_y )  #  the least squares solution

cat("\n\nregression_coefficients:\n")
print(w)

for_comparison = lm( medv ~ lstat * age, data=Boston )$coefficients

cat("\n\nlm coefficients for comparison:\n")
print(for_comparison)


# # OK!  Finally Questions for you to Answer!

# # PART A

# ## Linear Regression and Ridge Regression
# 
# Apparently data science job interviewers sometimes ask about "Ridge Regression" to see if candidates really know regression.
# Ridge Regression is really almost exactly the same thing as ordinary Linear Regression,
# but penalizes large coefficient values (with a weight constant $\lambda$ used in the penalty).
# After you see this is just a slight modification of Linear Regression,
# you have a deeper understanding of it.
# 
# By the way:  adding this penalty is called <b>regularization</b>
# (you are "regularizing" the regression objective function to respect other concerns).
# For some reason, just knowing this "regularization" term can be valuable.
# 
# As noted above, the linear regression coefficients $\boldsymbol{w}$ are defined by
# 
# $\boldsymbol{w} ~~=~~ (X'\,X)^{-1} \; X' \; \boldsymbol{y}$
# 
# In <b>Ridge Regression</b>, this is simply generalized to include a <b>regularization term</b> with a parameter $\lambda$:
# 
# $\boldsymbol{w} ~~=~~ (X'\,X \;+\; \lambda\,I)^{-1} \; X' \; \boldsymbol{y}$
# 
# Increasing $\lambda$ has the effect of reducing the scale of the regression coefficients:

# In[39]:

y = Boston$medv

X_ = as.matrix( subset(Boston, select = -medv) )

Xt_X = t(X_) %*% X_
Xt_y = t(X_) %*% y

minimum_lambda_value = 0
maximum_lambda_value = 100 ##  norm( Xt_X ) / 100000
number_of_lambda_values = 101

lambda_values = seq( minimum_lambda_value, maximum_lambda_value, length = number_of_lambda_values )

n = nrow(X_)
p = ncol(X_)

# keep track of all coefficient values -- for each value of lambda -- in a matrix:
coefficient_values_for_each_lambda = matrix(0, nrow=number_of_lambda_values, ncol=p)

colnames(coefficient_values_for_each_lambda) = colnames(X_)

I_p = diag(rep(1,p))  # pxp identity matrix  (rep(1,p) = a sequence with "1" repeated p times)

for (i in 1:number_of_lambda_values) {
    w = solve(  (Xt_X  +  I_p * lambda_values[i]),  Xt_y )
    coefficient_values_for_each_lambda[i,] = w
}
              
colors = rainbow( p, start=0.5, end=0.1 )  # generate p different colors, using hues from cyan (0.5) to orange (0.1)
              
plot( c(), c(), type="n",
     main="(ridge) regression coefficients, as a function of lambda",
     xlab="lambda", ylab="coefficient value",
     xlim=range(lambda_values)*1.02,
     ylim=range(coefficient_values_for_each_lambda) )

for (j in 1:p) {
    points( lambda_values, coefficient_values_for_each_lambda[,j],
           type="o", pch=20, cex=0.5, col=colors[j] )
    text( max(lambda_values), coefficient_values_for_each_lambda[number_of_lambda_values,j],
         colnames(X_)[j], pos=4, cex=0.75, col=colors[j] )  # put name of each variable at the end
}


# ## Your job is to find the regression coefficient that changes the most (has the maximum range) over the 101 lambda values between 0 and 100.
# 
# Each coefficient has a range of values (obtained over all 101 lambda values between 0 and 100).
# Your job is to find the coefficient with maximum range.
# 
# If each coefficient has range
# 
#     [min_value, max_value]
#     
# your job is to find the one for which the difference
# 
#     max_value - min_value
# 
# is maximum.  You should print the <b>column number</b> (variable number) of this coefficient.
# 
# For the Boston dataset, the <b>nox</b> variable has largest range (=3.024...),
# and it is the 5th column, so your program should print:
# <pre>
# 5
# </pre>

# In[40]:

# compute the range of coefficient values  (over all values of lambda considered)
max_coefficient_value = apply( coefficient_values_for_each_lambda, 2, max )
min_coefficient_value = apply( coefficient_values_for_each_lambda, 2, min )

coefficient_value_range = max_coefficient_value - min_coefficient_value
cat("\n\ncoefficient value range:\n")
print( coefficient_value_range )


# # PART B

# ## Find the observations that have greatest influence on the Regression coefficients
# 
# Suppose the $n \times p$ dataset we have is $X$.
# 
# Let $\bar{\boldsymbol{x}}$ be the $1 \times p$ row vector of column means of $X$,
# so its $j$-th entry is the average of the $j$-th column of $X$.
# 
# We want to determine the influence of the $i$-th observation $\boldsymbol{x}_i$ (row) of $X$
# on either the regression coefficients ($\boldsymbol{w}$)
# or the predicted $y$-values ($\hat{\boldsymbol{y}} \,=\, X\,\boldsymbol{w}$).
# 
# <b>THERE ARE NO INTERCEPTS IN THIS PROBLEM</b>:  $\boldsymbol{w}$ only contains coefficients for columns in $X$.
# 
# Define the following:
# 
# $X_{(i)}$ is the data with its $i$-th row ($\boldsymbol{x}_i$) <b>omitted</b>.
# 
# $\boldsymbol{y}_{(i)}$ is the $y$-vector with its $i$-th value <b>omitted</b>.
# 
# $\boldsymbol{w}_{(i)}$ are the regression coefficients computed for $X_{(i)}$ and $\boldsymbol{y}_{(i)}$;
# i.e.,  $\boldsymbol{w}_{(i)} \;=\; ({X_{(i)}}' X_{(i)})^{-1} ~ {X_{(i)}}' ~ \boldsymbol{y}_{(i)}$.
# 
# $\hat{\boldsymbol{y}}_{(i)} \;=\; X \,\boldsymbol{w}_{(i)}$ --- the  $y$ values predicted from the coefficients $\boldsymbol{w}_{(i)}$.
# 
# 
# People often define two measures of influence of the $i$-th observation:
# 
# (1) $Leverage_i ~~ = ~~ (\boldsymbol{x}_i - \bar{\boldsymbol{x}}) ~ C^{-1} ~ (\boldsymbol{x}_i - \bar{\boldsymbol{x}})'$,
# where $C$ is the <b>covariance matrix</b> of $X$.
# 
# (2) $CookDistance_i ~~=~~ (\hat{\boldsymbol{y}} - \hat{\boldsymbol{y}}_{(i)})' ~ (\hat{\boldsymbol{y}} - \hat{\boldsymbol{y}}_{(i)})$.
# 
# Because
# 
# $(\hat{\boldsymbol{y}} - \hat{\boldsymbol{y}}_{(i)}) ~ = ~  X ~ ({\boldsymbol{w}} - {\boldsymbol{w}_{(i)}})$
# 
# the Cook distance measure is also a measure of influence of the $i$-th row on the regression coefficients $\boldsymbol{w}$.
# 
# 
# Your job is to find the index $i$ of the rows ($\boldsymbol{x}_i$ and $\boldsymbol{y}_i$)
# that maximize $Leverage_i$ and $CookDistance_i$.
# 
# For example, with the Boston data, the value of $i$ that maximizes $Leverage_i$ is 381:

# In[41]:

X_ = subset(Boston, select = -medv) # all except the medv column

# X_ = subset( iris, select = -Species ) # all except the Species column

X_minus_Xbar = scale(X_, center=TRUE, scale=FALSE)
C_inverse = solve( cov(X_) )

Leverage = diag( (X_minus_Xbar) %*% C_inverse %*% t(X_minus_Xbar) )  ## very wasteful -- we just want the diagonal

i = which( Leverage == max(Leverage) )

cat("\n\nthe value of i that maximizes Leverage[i] is:\n")
cat(i, "\n")

cat("\n\nLeverage[i] is:\n")
print(Leverage[i])

cat("\n\nthe i-th row in X is:\n")
print( X_[i,] )


# # That's it!  Now write an R function that does this for any numeric X matrix and y vector
# 
# Your R script can be an extension of this outline:
# 
# <pre>
# Linear_Regression = function(X, y) {
# 
#     #  X is a nxp numeric matrix
#     #  y = is a nx1 numeric vector
# 
#     cat( ... number of variable whose coefficient has the maximum range ..., "\n" )
# 
#     cat( ... the value of i that maximizes  Leverage[i] ..., "\n" )
# 
#     cat( ... the value of i that maximizes  CookDistance[i] ..., "\n" )
#     
# }
# </pre>

# # What your function's output should look like
# 
# If the Boston table above was given as input,
# your program should print the following values:
# <pre>
# 5
# 381
# 369
# </pre>
# 
# 
# As another test, with the built-in <tt>mtcars</tt> dataset, the output should look like this:
# <pre>
# 6
# 9
# 29
# </pre>
# 
# As usual, output values are always integers.

# #  Before submitting it, test your function on the <tt>mtcars</tt> dataset
# 
# 
# ### 1. Try your function on the "mtcars" dataset.
# 
# As another test, with the <tt>mtcars</tt> dataset (which is a builtin dataset in R).
# 
# <pre>
#    X = subset( mtcars, select = -mpg )
#    y = subset( mtcars, select =  mpg )
# 
#    Linear_Regression(X,y)
# </pre>
# 
# ### 2. Also try your function on the "mtcars" dataset, after replacing the <tt>mpg</tt> column by <tt>1/mpg</tt>:
# 
# <pre>
#    # inverted_mtcars = transform( mtcars, mpg = 1/mpg )  # this could be one way to do it
# 
#    X = subset( mtcars, select = -mpg )
#    y = subset( mtcars, select =  mpg )
#    y = 1/y
# 
#    Linear_Regression(X,y)
# </pre>
# 
# ## Upload to CCLE both your R function file <tt>LinearRegression.R</tt> and its output for these tests.
# 
# <!--
# #  Before submitting it, test your function on two datasets:
# 
# ### 1. Try your function on the "vowels" dataset.
# 
# <pre>
# data(mtcars)
# 
# Linear_Regression( subset(mtcars, select=-mpg), subset(mtcars, select=mpg) )
# ##  'mpg', the first column, is a numeric value
# 
# 
# </pre>
# 
# ### 2. Try your function on the "spam" dataset.
# 
# <pre>
# spam = read.csv("spam.csv", header=TRUE)  # spam.csv is included with this assignment
# 
# Linear_Regression( subset(spam, select=-is_spam), subset(mtcars, select=is_spam) )
# </pre>
# 
# You can include your output for these two tests in your notebook here,
# or in some separate file.
# Submit this output along with the rest of your HW3 materials.
# 
# ## Then:  Upload to CCLE both your R function file <tt>LinearRegression.R</tt> and its output for these two test datasets.
# -->

# In[ ]:



