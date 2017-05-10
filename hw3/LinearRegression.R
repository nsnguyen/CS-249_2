Linear_Regression = function(X, y){
  not.installed = function(package_name)  !is.element(package_name, installed.packages()[,1])
  if (not.installed("MASS")) install.packages("MASS")
  if (not.installed("ISLR")) install.packages("ISLR")
  library(MASS)
  library(ISLR)
  
  #  X is a nxp numeric matrix
  #  y = is a nx1 numeric vector
  
  X_ = as.matrix(X)
  y_ = as.matrix(y)
  
  Xt_X = t(X_) %*% X_
  Xt_y = t(X) %*% y_
  
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
  
  #cat( ... number of variable whose coefficient has the maximum range ..., "\n" )
  range = apply(coefficient_values_for_each_lambda,2,range)
  p = ncol(range)
  largest_range = 0
  col_index = 0
  for (i in 1:p){
    if(abs(max(range[,i])-min(range[,i])) > largest_range){
      largest_range = abs(max(range[,i])-min(range[,i]))
      col_index = i
    }
  }
  cat(col_index, "\n")
  
  
  #CACULATE LEVERAGE DISTANCE############################################
  #cat( ... the value of i that maximizes  Leverage[i] ..., "\n" )
  X_minus_Xbar = scale(X_, center=TRUE, scale=FALSE)
  C_inverse = solve( cov(X_) )
  Leverage = diag( (X_minus_Xbar) %*% C_inverse %*% t(X_minus_Xbar) )  ## very wasteful -- we just want the diagonal
  i = which( Leverage == max(Leverage) )
  cat(i, "\n")
  
  
  
  #CALCULATE COOK DISTANCE############################################
  #cat( ... the value of i that maximizes  CookDistance[i] ..., "\n" )
  irow = nrow(y_) #should be the same as X_       
  result_ = 0
  i_ = 0
  
  #caculate w
  w =  solve(t(X_) %*% X_) %*% t(X_) %*% y_
  
  for (i in 1:irow){
    #w(i)=(X(i)′X(i))−1 X(i)′ y(i) to caculuate w_i
    w_i = solve(t(X_[-i,]) %*% X_[-i,]) %*% t(X_[-i,]) %*% y_[-i,]
    
    #(ŷ − ŷ(i)) = X (w−w(i))(y^−y^(i)) = X (w−w(i))        
    diff_y_hat = X_ %*% (w - w_i)
    
    #cook distance: (ŷ −ŷ (i))′ (ŷ −ŷ (i))
    temp_result = abs(t(diff_y_hat) %*% diff_y_hat)
    
    if(temp_result > result_){
      #check for highest value of Cook distance.
      result_ = temp_result
      i_ = i
    }       
  }      
  cat(i_, "\n")
  
}

print('Boston')
X = subset(Boston, select = -medv)
y = subset(Boston, select = medv)
Linear_Regression(X,y)

print('mtCars')        
X = subset( mtcars, select = -mpg )
y = subset( mtcars, select =  mpg )
Linear_Regression(X,y)

print('invereted mtCars')   
X = subset( mtcars, select = -mpg )
y = subset( mtcars, select =  mpg )
y = 1/y
Linear_Regression(X,y)