PCA = function(Table){
  not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])
  if (not.installed("ISLR"))  install.packages("ISLR")
  library(ISLR)  #  load the ISLR package
  
  #computer principal components analysis (PCA)
  pr.out= prcomp(Table, scale=TRUE) #scaling variables before  performing PCA
  
  #print(pr.out$rotation)
  
  #principale components (PC) are only unique up to a sign change
  pr.out$rotation = -pr.out$rotation
  pr.out$x = -pr.out$x 
  
  #calculate the variance
  pr.var = pr.out$sdev^2
  
  #compute the proportion of variance explained by each principal component
  pve = pr.var/sum(pr.var)
  
  #Part A - Rule 1: Keep enough PCs to explain 70% of the variance
  total_variance = 0
  for(i in 1:length(pve)){
    total_variance = total_variance + pve[i]
    if (total_variance >= (70/100)){
      #     cat( ... number of principal components to keep by Rule 1 ..., "\n" )
      cat(i, "\n")
      break
    }
  }
  
  #Part A - Rule 2: Keep All PCs whose correlation matrix eigenvalues are greater than 1
  lambda = svd(cor(Table))$d
  
  total_lambda = 0   
  for(i in 1:length(lambda)){
    
    if(lambda[i] >= 1){
      #     cat( ... number of principal components to keep by Rule 2 ..., "\n" )
      total_lambda = total_lambda + 1
    }
  }
  cat(total_lambda, "\n")
  
  #Part B- Each PC is an eigenvector u. It has p entries for each variable.
  #We say the q strongest loadings of u are the largest q entries u^2 whose sum exceeds 0.7
  
  PrincipalComponents = svd(cor(Table))$u
  ComponentEntryRanks = apply(-PrincipalComponents^2, 2, rank)       
  SortedSquaredComponents = apply(PrincipalComponents^2, 2, function(x) sort(x, decreasing=TRUE))
  CumulativeSortedSquaredComponents = apply( SortedSquaredComponents, 2, cumsum)
  
  q_strongest_loadings = vector()   
  for(i in 1:ncol(CumulativeSortedSquaredComponents)){       
    for(j in 1:nrow(CumulativeSortedSquaredComponents)){
      if(CumulativeSortedSquaredComponents[j,i] > 0.7){
        q_strongest_loadings = append(q_strongest_loadings,j)
        break
      }
    }
  }   
  #     cat( ... vector of numbers of strongest loadings for each PC, as defined above ..., "\n" )
  cat(q_strongest_loadings, "\n")
  
  
  
  #Part C - Find the observations that have greatest influence on the PCs   
  U = PrincipalComponents
  Z = scale(Table, center=TRUE, scale=FALSE) %*% U
  lambda = rep(1, nrow(Z)) %*% t(lambda)
  
  #     cat( ... the value of i that maximizes  a[i] ..., "\n" )
  a = apply(Z^2, 1, sum)
  i = which(a == max(a))
  cat(i, "\n")
  
  #     cat( ... the value of i that maximizes  b[i] ..., "\n" )
  b = apply(Z^2/lambda, 1, sum)
  i = which(b == max(b))
  cat(i, "\n")
  
  #     cat( ... the value of i that maximizes  c[i] ..., "\n" )
  c = apply(lambda*Z^2, 1, sum)
  i = which(c == max(c))
  cat(i, "\n")
  
}

Table = data.matrix(read.csv( file("stdin"), header=TRUE ))
PCA(Table)