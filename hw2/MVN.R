
g = function(xvec, meanvec, inverseCovMatrix, Sigma) {
  #here is the Gaussian PDF
  k <- nrow(Sigma)
  detSigma = det(Sigma)
  return (1 / (2*pi)^(k/2) / sqrt(detSigma) *
            exp( -1/2 * ( t(xvec-meanvec) %*% inverseCovMatrix %*% (xvec-meanvec) )[1,1] ) )
}

g_distance = function(xvec, meanvec,inverseCovMatrix){
  #distance part is taken from g function above
  return ( t(xvec-meanvec) %*% inverseCovMatrix %*% (xvec-meanvec) )[1,1]
}

MVN = function(Table){
  
  #data.matrix() forces data to be numeric
  X = data.matrix(Table[,1:(ncol(Table)-1)]) 
  
  #determine the classification (assuming that last column is the classifier)
  classifications = Table[, ncol(Table)]
  
  #get unique length of classifications
  #k is the number of different classifications
  k = length(unique(classifications))
  
  #the class values will always intergers from 1 to k here
  #need to unclass the classifications
  y = unclass(classifications)
  
  #get number of rows
  n = nrow(X)
  #get number of columns
  p = ncol(X)
  
  Sigma_Cov = cov(X)
  
  Inverse_Sigma_Cov = solve(Sigma_Cov)
  
  #matrix to record distance values
  distance_value = matrix(0, nrow=n, ncol=k)
  
  # ... For each class j from 1 to k
  for(j in 1:k){
    # ...    Derive the MVN distribution parameters for the j-th class.  
    #getting subset of data for each j class
    subSet_Data_for_j_class = subset(X,(y==j))
    
    mean_vector = matrix( apply(subSet_Data_for_j_class, 2, mean), nrow=p, ncol=1 )  # column vector     
    
    #setting Covariance matrix
    cov_matrix = cov(subSet_Data_for_j_class)
    
    #setting Covariance matrix inverse
    cov_matrix_inverse = solve(cov_matrix)
    
    #setting determinant of Covariance matrix
    cov_det = det(cov_matrix)
    
    # ...    For each row x[i,] in the X matrix,
    for(i in 1:n){
      each_row_in_X = X[i,]
      
      # ...       distance_value[i,j] = the Gaussian distance_value of x[i,]                    
      distance_value[i,j] = g(each_row_in_X, mean_vector, cov_matrix_inverse, cov_matrix)
      #distance_value[i,j] = g_distance(each_row_in_X, mean_vector, cov_matrix_inverse)
    }
  }
  # ... For each row x[i,] in the X matrix, 
  for(i in 1:n){
    # ...    If jmin is the number of this closest class and is different from y[i],  
    
    #use max if g function is used
    #jmin = which(distance_value[i,] == max(distance_value[i,]))
    jmin = which.max(distance_value[i,])
    
    #use min when calculating distance.
    #jmin = which(distance_value[i,] == min(distance_value[i,]))
    
    #ifelse(jmin != y[i], cat("%s %s %s\n", i, jmin, y[i]),'')   
    
    if(jmin != y[i]){
      cat(sprintf("%s %s %s\n", i, jmin, y[i]))
    }     
  }  
}

Table = data.matrix(read.csv( file("stdin"), header=TRUE ))
MVN(Table)

#print('Iris')
#MVN(iris)

