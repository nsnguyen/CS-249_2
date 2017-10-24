# Iteratively Reweighted Least Squares implementation of Logistic Regression
# This simple implementation assumes there are no missing values,
#   and all y values are either 0 or 1.

# Below, the variable 'p' is often referred to as 'mu'  (mean of the link function).
# Also, 'yhat' is often referred to as 'eta' (link function).

rm(list=ls(all=TRUE))

logistic_regression = function( formula, dataset, tolerance=1.0e-6 ) {
  #(a) First, modify the program to delete all rows of the data that have missing values,
  #dataset_omit = na.omit(dataset) # Delete rows that have null values.
  
  #(c) Drop columns with more than 10% missing values.
  dataset_omit = within(dataset, rm(
    talented
    , security
    , haircut
    , sexual_orientation
    , weight
    , political_affiliation
    , rehab
    , plastic_surgery
    , intoxicated
    , wealth
    , age_well
    , outfit
    , trustworthy
    , dress_size
    , hire
    , intelligence))
  dataset_omit = na.omit(dataset_omit)
  
  initial.model = model.frame( formula, dataset_omit )
  X = model.matrix( formula, data = dataset_omit )
  y = model.response( initial.model, "numeric" )  # y values should be 0 and 1
  p = ifelse( y==0, 0.25, 0.75 )   # initial values; all y values are 0 or 1
  yhat = log(p/(1-p))
  prev_deviance = 0
  deviance = 2*sum( y*log(1/p) + (1-y)*log(1/(1-p)) )
  while (abs(deviance - prev_deviance) > tolerance) {
    w = p * (1-p)
    ynew = yhat + (y-p)/w
    model = lm( ynew ~ X - 1,  weights = w )   #  weighted least squares
    yhat = model$fit
    p = 1/(1 + exp(-yhat))
    prev_deviance = deviance
    deviance = 2 * sum( y*log(1/p) + (1-y)*log(1/(1-p)) )
   }
   rss = sum( residuals( model, type="pearson")^2 )  #  weighted RSS
   dispersion = rss / model$df.residual
   return(list( coef = coef(model),  stderr = sqrt( diag(vcov(model)) ) / sqrt(dispersion)  ))
}

#demo = function() {
#   data(iris)
#  #read.csv('Users/nnguyen/Documents/_Dev/CS-249_2/final_exam/attractiveness/facestat_train.csv')
#   zero_one_iris = transform( iris,  Species = ifelse( unclass(Species)==2, 0, 1 ) )
#   print(logistic_regression( Species ~ ., zero_one_iris ))
#}

data = read.csv('/Users/nnguyen/Documents/_Dev/CS-249_2/final_exam/attractiveness/facestat_train.csv')
logistic_regression( male ~ ., data )


#a) Deleting all rows that have nulls will return a datafram with no rows.
#This is true because all rows have some kind of null values.

#b) Can't run a model without values. Therefore, we cannot simply delete all rows.

#c) basing on the attached notebook, we know that each column has various missing NA percentage.
#Therefore, we can simply drop all columns with more than 30% missing values.
# talented: 99.15
# security: 99.6
# haircut:99
# sexual_orientation: 90.85
# attractive: 8.45
# dogfight: 97.4
# weight: 50.4
# political_affiliation:69.4
# rehab: 99.7
# plastic_surgery: 99.5
# intoxicated: 76.35
# wealth: 85.85
# age_well: 99.5
# outfit: 99.5
# trustworthy: 58
# dress_size: 99.6
# hire: 99.35
# intelligence: 30.7
# age: 0
# male: 0

#d) This is a better model than deleting all rows as seen in part a above which could potentially delete all rows.
# We are able to preserve more rows with this way, however we had to drop the columns with more than 10% missing values.
# a better way to do this is to impute the missing values instead of dropping them. We can further see this in the attached notebook
# which uses the MICE package to impute missing NA values. The prediction for gender will be using the method in the notebook instead
# which has 70% accuracy.

# $coef
# X(Intercept)  Xattractive    Xdogfight         Xage 
# 8.36248302  -1.34783859  -2.19469823   0.02500393 
# 
# $stderr
# X(Intercept)  Xattractive    Xdogfight         Xage 
# 4.05814525   1.01647650   0.94589577   0.06598321 
