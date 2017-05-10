
# coding: utf-8

# # HW3 Problem #1: PCA
# 
# ## Due Sunday May 7, at 11:55pm
# 
# ## However:  it is strongly recommended you master this material before the Midterm on Saturday May 6.  The Midterm will cover this material.

# <hr style="height: 30px; background-color: gray;"/>

# ## Reproduced here for convenience --  Tutorial on PCA  -- from [ISL] Ch.10
# 

# In[44]:

not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])

if (not.installed("ISLR"))  install.packages("ISLR")

library(ISLR)  #  load the ISLR package

print(help(USArrests))  #  information about the USArrests dataset
    
# This dataset is studied in Chapter 10 of the [ISL] text.
#  Section 10.2 is about PCA.

pairs(USArrests)


# ## Lab 1 from Chapter 10: Principal Components Analysis

# In[45]:

states = row.names(USArrests)
print(states)

print( names(USArrests) )


# In[46]:

print( apply(USArrests,  2,  mean) )
print( apply(USArrests,  2,  var) )


# In[47]:

pr.out = prcomp(USArrests,  scale = TRUE)

print( names(pr.out) )
print( pr.out$center )
print( pr.out$scale )
print( pr.out$rotation )
print( dim(pr.out$x) )

biplot(pr.out,  scale = 0)


# In[48]:

print( pr.out$rotation )

# perform a mirror-image transformation of the plot
#  (signs of principal components/eigenvectors do not matter)

pr.out$rotation = -pr.out$rotation  #  negate all eigenvectors/PCs
pr.out$x = -pr.out$x  # also negate all coordinate values

biplot(pr.out,  scale = 0)  # should be the mirror image


# In[49]:

print( pr.out$sdev )
pr.var = pr.out$sdev^2
print( pr.var )

pve = pr.var/sum(pr.var)
# eigenvalues divided by total = proportion of variance explained [pve]
print( pve )

plot(pve,  xlab = "Principal Component",  ylab = "Proportion of Variance Explained",  ylim = c(0, 1), type = 'b')


# In[50]:

plot(cumsum(pve),  xlab = "Principal Component",  ylab = "Cumulative Proportion of Variance Explained",  ylim = c(0, 1), type = 'b')
a = c(1, 2, 8, -3)
cumsum(a)


# In[51]:

print( pr.out$rotation )

heatmap( pr.out$rotation,
        main="Loadings (cyan positive, magenta negative)",
        sub="PC2 ~= (UrbanPop - Murder), PC3 ~= (-Rape), PC4 ~= (Assault - Murder)",
        col=rev(cm.colors(256)),
        scale="none", rev=TRUE, symm=FALSE,
        Rowv=NA, Colv=NA, mar=c(5,10) )


# ## How to do PCA using only the SVD

# In[52]:

print(cov(USArrests))

PCAsvd = svd( cov(USArrests) )

U = PCAsvd$u
S = diag(PCAsvd$d)
V = PCAsvd$v

cat("\n\nU:\n")
print(U)

cat("\n\nS:\n")
print(S)

cat("\n\nV:\n")
print(V)

cat("\n\nerror in SVD reconstruction:\n")
print( norm( cov(USArrests) - U %*% S %*% t(V) ) )


# In[53]:

PCAsvd = svd( cov(USArrests) )

PrincipalComponents = PCAsvd$u  #  i.e.:  U
colnames(PrincipalComponents) = c( "PC1", "PC2", "PC3", "PC4" )

Eigenvalues = PCAsvd$d          #  i.e.:  diag(S)

cat("\n\nPrincipal Components (columns of the U eigenvector matrix):\n")
print(PrincipalComponents)

cat("\n\nEigenvalues  (diagonal entries of the S eigenvalue matrix):\n")
print(Eigenvalues)

cat("\n\nTrace of covariance matrix = sum of its eigenvalues:\n")
print( sum(diag( cov(USArrests) )))
print( sum( Eigenvalues) )

cat("\n\nNotice:  every eigenvalue of a covariance matrix is nonnegative!")


# In[54]:

# PercentageOfVarianceExplainedByPC1 = Eigenvalues[1]/sum(Eigenvalues)
# PercentageOfVarianceExplainedByPC2 = Eigenvalues[2]/sum(Eigenvalues)
# PercentageOfVarianceExplainedByPC3 = Eigenvalues[3]/sum(Eigenvalues)
# PercentageOfVarianceExplainedByPC4 = Eigenvalues[4]/sum(Eigenvalues)

PercentageOfVarianceExplained = Eigenvalues / sum(Eigenvalues)
cat("\n\nPercentage of variance explained:\n")
print(PercentageOfVarianceExplained)
plot(PercentageOfVarianceExplained, type="o", col="red")


# In[55]:

# PercentageOfVarianceExplainedByPC1andPC2 = sum(Eigenvalues[1:2])/sum(Eigenvalues)

CumulativePercentageOfVarianceExplained = cumsum( PercentageOfVarianceExplained )
cat("\n\nCumulative percentage of variance explained:\n")
print(CumulativePercentageOfVarianceExplained)
plot(CumulativePercentageOfVarianceExplained, type="o", col="blue")


# <hr style="height: 30px; background-color: gray;"/>

# ## Dimensionality Reduction
# 
# We can reduce the dimensionality of the data by projecting the data onto the first few principal components of the data.
# 
# Notice that the first eigenvalue explains 96% of the variance in the data, and the first two cumulatively explain 99% of the variance.
# 
# If project the data onto the first two principal components, we get a 2D representation of the data.  This is what is shown in the "biplot" above.

# In[56]:

FirstTwoPrincipalComponents = PrincipalComponents[, 1:2]
# The first two PCs are the first 2 eigenvectors of the covariance matrix

n = nrow(USArrests)
CenteredData = scale(USArrests, center=TRUE, scale=FALSE)

# We can project the data onto these components with matrix multiplication:
ProjectionOfTheData  =  CenteredData %*% FirstTwoPrincipalComponents

xvalues = ProjectionOfTheData[,1]
yvalues = ProjectionOfTheData[,2]

plot(xvalues, yvalues, pch=20, col="blue")

text(xvalues, yvalues, row.names(USArrests), pos=3, col="black", cex=0.75)



# ## Using the Correlation Matrix instead of the Covariance Matrix
# 
# If the data columns are not scaled in similar ways, the covariance matrix will emphasize variables having larger scale.  This can easily lead to misinterpretation of the results.
# 
# A relatively safe strategy in PCA is to <b>use the correlation matrix</b>
# (instead of the covariance matrix), unless there is a clear reason to use
# some other scaling.  The correlation matrix is the covariance matrix of the standardized (normalized, scaled, z-score) version of the data.
# 
# <b>As an example, we can re-do the analysis above for USArrests.
# The analysis above, from the [ISL] text, may be misleading about the data.</b>
# 

# In[57]:

PCAsvd = svd( cor(USArrests) )   ############# NOTE!  Correlation matrix

PrincipalComponents = PCAsvd$u  #  equivalently:  U

colnames(PrincipalComponents) = c( "PC1", "PC2", "PC3", "PC4" )
rownames(PrincipalComponents) = colnames(USArrests)

Eigenvalues = PCAsvd$d       #  equivalently:  diag(S)


cat("\n\nPrincipal Components (columns of the U eigenvector matrix):\n")
print(PrincipalComponents)

cat("\n\nEigenvalues  (diagonal entries of the S eigenvalue matrix):\n")
print(Eigenvalues)


# In[58]:

PercentageOfVarianceExplained = Eigenvalues / sum(Eigenvalues)
cat("\n\nPercentage of variance explained:\n")
print(PercentageOfVarianceExplained)
plot(PercentageOfVarianceExplained, type="o", col="red")

PercentageOfVarianceExplainedByPC1andPC2 = sum(Eigenvalues[1:2])/sum(Eigenvalues)

CumulativePercentageOfVarianceExplained = cumsum( PercentageOfVarianceExplained )
cat("\n\nCumulative percentage of variance explained:\n")
print(CumulativePercentageOfVarianceExplained)
plot(CumulativePercentageOfVarianceExplained, type="o", col="blue")


# In[59]:

FirstTwoPrincipalComponents = PrincipalComponents[, 1:2]
# The first two PCs are the first 2 eigenvectors of the covariance matrix

n = nrow(USArrests)
CenteredData = scale(USArrests, center=TRUE, scale=FALSE)

# We can project the data onto these components with matrix multiplication:
ProjectionOfTheData  =  CenteredData %*% FirstTwoPrincipalComponents

xvalues = ProjectionOfTheData[,1]
yvalues = ProjectionOfTheData[,2]

plot(xvalues, yvalues, pch=20, col="blue",
     main = "SIMILAR, YET DIFFERENT, PCA RESULTS!")

text(xvalues, yvalues, row.names(USArrests), pos=3, col="black", cex=0.75)


# In[60]:

print( PrincipalComponents )

heatmap( PrincipalComponents,
        main="Loadings (cyan positive, magenta negative)",
        sub="PC2 ~= (-UrbanPop + Murder), PC3 ~= (+Rape), PC4 ~= (-Assault + Murder)",
        col=rev(cm.colors(256)),
        scale="none", rev=TRUE, symm=FALSE,
        Rowv=NA, Colv=NA, mar=c(5,10) )


# In[ ]:




# # OK!  Finally Questions for you to Answer!

# # PART A

# ## How many Principal Components should we keep?
# 
# For the USArrests data we kept 2 PCs, but there are different rules for how many PCs to keep.
# 
# <u><b>Assume we always use the correlation matrix for PCA</b></u>:
# 
# <u><b>Rule#1</b></u>:  <i>keep enough PCs to explain 70% of the variance</i>.<br/>
# In other words, keep k PCs, where k is the smallest integer such that:
# $$ \sum_{j=1}^k \, \lambda_j ~~~ \geq ~~~ 0.7 ~ \sum_{j=1}^p \, \lambda_j ~~~ = ~~~ 0.7 ~ p. $$
# Notice that the trace of the correlation matrix is $p$, so
# $\sum_{j=1}^p \lambda_j \,=\,p$.
# 
# <u><b>Rule#2</b></u>:  <i>keep all PCs whose correlation matrix eigenvalues are greater than 1</i>.<br/>
# In other words, if $\lambda_j$ is the $j$-th eigenvalue from the <b>correlation matrix</b>,
# keep $k$ PCs, where $k$ is the smallest integer such that
# $$ \lambda_k ~~ \geq ~~ 1. $$
# 
# For example, for the USArrests data, Rule#1 says we should keep 2 PCs,
# while Rule#2 says we should keep 1 PC:

# In[61]:

lambda = svd( cor(USArrests) )$d
p = ncol(USArrests)

cat("\n\nEigenvalues of the correlation matrix, in descending order\n")
print(lambda)

cat("\n\ncumulative sums of eigenvalues\n")
print( cumsum(lambda)/p )

cat("\n\nEigenvalues >= 1\n")
print( lambda >= 1 )


# # PART B

# ## For each PC (principal component), which variables have the strongest loadings?
# 
# Each PC is an eigenvector $\boldsymbol{u}$.  It has $p$ entries, one for each variable.
# 
# The loading of variable $j$ ($1 \leq j \leq p$)
# on a principal component $\boldsymbol{u}$ is the $j$-th entry $u_{j}$.  These entries can be positive or negative.  Since $\boldsymbol{u}$ is an eigenvector, however, the sum of squares of its entries is 1.
# 
# We say the $q$ <i>strongest loadings</i> of $\boldsymbol{u}$ are
# the largest $q$ entries $u_j^2$ whose sum exceeds 0.7.

# In[62]:

PrincipalComponents = svd(cor(USArrests))$u  ### correlation matrix

colnames(PrincipalComponents) = c( "PC1", "PC2", "PC3", "PC4" )
rownames(PrincipalComponents) = colnames(USArrests)

cat("\n\nThe pxp matrix of PCs:\n")

print(PrincipalComponents)

cat("\n\nSquare each entry in the matrix of PCs:\n")

print(PrincipalComponents^2)

# notice that the first column has q=3 strong loadings, since we
# need to use the 3 largest squared values to obtain a sum above 0.7.

# Similarly the second column has q=1 strong loadings, because
# one its squared values is already above 0.7.


# The following kinds of functions are useful for computing the strongest loadings of the principal components:

# In[63]:

cat("\n\nThe pxp matrix of PCs:\n")

print(PrincipalComponents)

cat("\n\nSquare each entry in the matrix of PCs:\n")

print(PrincipalComponents^2)

cat("\n\nApply the 'rank' function to each column:\n")

ComponentEntryRanks = apply( -PrincipalComponents^2, 2, rank )
print(ComponentEntryRanks)

cat("\n\nApply the 'sort' function to each column:\n")

SortedSquaredComponents = apply(PrincipalComponents^2, 2, function(x) sort(x, decreasing=TRUE))
print(SortedSquaredComponents)

cat("\n\nApply the 'cumsum' function to each column:\n")
    
CumulativeSortedSquaredComponents = apply( SortedSquaredComponents, 2, cumsum)
print(CumulativeSortedSquaredComponents)


# # PART C

# ## Find the observations that have greatest influence on the PCs
# 
# Suppose the $n \times p$ dataset we have is $X$, and its <b>correlation matrix</b> $C$ has the SVD
# $$C ~~=~~ U ~ S ~ U'$$
# so that the $U$ is the $p \times p$ matrix of principal components,
# and $S = \mbox{diag}(\lambda_1, ..., \lambda_p)$ is the diagonal matrix of eigenvalues.
# 
# Let $\bar{\boldsymbol{x}}$ be the $1 \times p$ row vector of column means of $X$,
# so its $j$-th entry is the average of the $j$-th column of $X$.
# 
# Let $\bar{X}$ be the $n \times p$ matrix of column means of $X$,
# so its $j$-th column consists of $n$ copies of the average of the $j$-th column of $X$.
# 
# Finally, define $Z \;=\; (X-\bar{X})\,U$.  This is the <b>projection of the data on the principal components</b>.
# 
# People often define 3 measures of influence of the $i$-th observation $\boldsymbol{x}_i$ (row) of $X$:
# 
# (1) $a_i ~~=~~ \sum_{j=1}^p z_{ij}^2$, the sum of squares of the $i$-th row of $Z$.
# We can show that $a_i = (\boldsymbol{x}_i - \bar{\boldsymbol{x}})' \,  (\boldsymbol{x}_i - \bar{\boldsymbol{x}})$.
# 
# (2) $b_i ~~=~~ \sum_{j=1}^p z_{ij}^2/\lambda_{j}$.
# We can show that $b_i = (\boldsymbol{x}_i - \bar{\boldsymbol{x}})' \, C^{-1} \, (\boldsymbol{x}_i - \bar{\boldsymbol{x}})$.
# 
# (3) $c_i ~~=~~ \sum_{j=1}^p \lambda_{j} ~ z_{ij}^2$.
# We can show that $c_i = (\boldsymbol{x}_i - \bar{\boldsymbol{x}})' \, C \, (\boldsymbol{x}_i - \bar{\boldsymbol{x}})$.
# 
# The $a$-values emphasize the size of each row;
# the $b$-values emphasize influence on small principal components;
# the $c$-values emphasize influence on large principal components.
# 
# Your job is to find the index $i$ of the observations $\boldsymbol{x}_i$ that maximizes $a_i$, $b_i$, and $c_i$.
# 
# For example, with the USArrests data, the value of $i$ that maximizes $a_i$ is 33:

# In[64]:

U = PrincipalComponents
Z = scale(USArrests, center=TRUE, scale=FALSE) %*% U

cat("\n\na-influence values (apply sum to each row of squared Z matrix):\n")
a = apply(Z^2, 1, sum )
print(a)

cat("\n\nfind the value of i that maximizes a[i]:\n")
i = which( a == max(a) )

cat( i, "\n")

cat("\n\nthe value of a[i]:\n")
print(a[i])


# # That's it!  Now write an R function <tt>PCA(Table)</tt> that does this for any dataset.
# 
# Your R function can be an extension of this outline:
# 
# <pre>
# PCA = function(Table) {
# 
#     cat( ... number of principal components to keep by Rule 1 ..., "\n" )
# 
#     cat( ... number of principal components to keep by Rule 2 ..., "\n" )
# 
#     cat( ... vector of numbers of strongest loadings for each PC, as defined above ..., "\n" )
# 
#     cat( ... the value of i that maximizes  a[i] ..., "\n" )
# 
#     cat( ... the value of i that maximizes  b[i] ..., "\n" )
# 
#     cat( ... the value of i that maximizes  c[i] ..., "\n" )
# 
# }
# </pre>

# # What your program's output should look like
# 
# If the USArrests table above was given as input,
# your program should print the following values:
# <pre>
# 2
# 1
# 3  1  2  2
# 33
# 33
# 9
# </pre>
# 
# 
# As usual, output values are always integers.

# #  Before submitting it, test your function on the <tt>mtcars</tt> dataset
# 
# 
# ### 1. Try your function on the "mtcars" dataset.
# 
# As another test, with the <tt>mtcars</tt> dataset (which is a builtin dataset in R),
# the output of your function should look like this:
# <pre>
# 2 
# 2 
# 6 4 4 1 3 4 2 2 3 1 2 
# 15 
# 15 
# 15 
# </pre>
# 
# ### 2. Also try your function on the "mtcars" dataset, after replacing the <tt>mpg</tt> column by <tt>1/mpg</tt>:
# 
# <pre>
# inverted_mtcars = transform( mtcars, mpg = 1/mpg )
# 
# PCA( inverted_mtcars )
# </pre>
# 
# ##  Upload to CCLE both your R function file <tt>PCA.R</tt> and its output for these tests.
# 
# <!--
# ### 1. Try your function on the "vowels" dataset.
# 
# <pre>
# vowels = read.csv("vowels.csv", header=TRUE)  # vowels.csv is included with this assignment
# 
# PCA(vowels)
# 
# ##  'y', the last column, is an integer between 1 and 11 (the classification of the vowel sound).
# ##   Use this value to display the data points in color.
# ##   You can obtain 11 colors with something like   <tt>rainbow(11, start=0.60, 0.95)</tt>
# </pre>
# 
# ### 2. Try your function on the "spam" dataset.
# 
# <pre>
# spam = read.csv("spam.csv", header=TRUE)  # spam.csv is included with this assignment
# 
# PCA(spam)
# 
# ##  'is_spam', the last column, is 2 if the classification is spam, 1 if not spam.
# ##   Use this value to display the data points in 2 colors.
# </pre>
# 
# You can include your output for these two tests in your notebook here,
# or in some separate file.
# Submit this output along with the rest of your HW3 materials.
# 
# ## Then:  Upload to CCLE both your R function file <tt>PCA.R</tt> and its output for these two test datasets.
# -->
# 

# In[ ]:



