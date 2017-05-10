LDA_and_QDA = function(Table){
  
  not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])
  if (not.installed("MASS"))  install.packages("MASS")  # we need the MASS package
  library(MASS) 
  
  # printing only the mean value/centroid for each class
  
  #remove class in last column
  X = data.matrix(Table[, 1:(ncol(Table)-1)])
  
  classifications = Table[, ncol(Table)]
  
  #putting unique classifications into classes
  classes = as.character( unique(classifications) )
  k = length(classes)
  
  y = unclass(classifications) #convert class to numeric indices
  
  n = nrow(X) #row
  p = ncol(X) #column
  
  
  # ... construct an LDA representation of X       
  LDA.model = lda(y ~ X)
  
  #print(LDA.model)
  
  # Compute the LDA predictions ("classifications") for each input row.
  LDA.classifications = predict(LDA.model, as.data.frame(X))$class
  
  #print(LDA.classifications)
  
  # ... determine for which rows in X the LDA classification differs from y
  LDA.disagreements = (1:nrow(X))[LDA.classifications != y]
  
  #print(LDA.disagreements)
  
  # Tabulate the number of LDA classification values vs. y values
  LDA.confusion.matrix = table(LDA.classifications, y)      
  rownames(LDA.confusion.matrix) = classes
  colnames(LDA.confusion.matrix) = classes
  
  #print(LDA.confusion.matrix)
  
  # ... print the confusion matrix for LDA
  for (i in 1:nrow(LDA.confusion.matrix)) {
    cat( LDA.confusion.matrix[i,], "\n" )  # print each row as a sequence
  }
  
  
  
  # ... construct a QDA representation of X
  QDA.model <- qda(y ~ X)    
  
  # Compute the LDA predictions ("classifications") for each input row.
  QDA.classifications = predict(QDA.model, as.data.frame(X))$class
  
  # ... determine for which rows in X the QDA classification differs from y
  QDA.disagreements = (1:nrow(X))[QDA.classifications != y]
  
  
  # Tabulate the number of LDA classification values vs. y values
  QDA.confusion.matrix = table(QDA.classifications, y)      
  rownames(QDA.confusion.matrix) = classes
  colnames(QDA.confusion.matrix) = classes  
  
  
  # ... print the confusion matrix for QDA
  for (i in 1:nrow(QDA.confusion.matrix)) {
    cat( QDA.confusion.matrix[i,], "\n" )  # print each row as a sequence
  }    
  
}

Table = data.matrix(read.csv( file("stdin"), header=TRUE ))
LDA_and_QDA(Table)


#print('Iris')
#LDA_and_QDA(iris)
