#Name: Nguyen Nguyen
#ID: 004870721

set.seed(2222)

not.installed <- function(pkg) !is.element(pkg, installed.packages()[,1])

if (not.installed("MASS"))  install.packages("MASS", repos = "http://cran.us.r-project.org")  # we need the MASS package

#load package
library(MASS)

##getwd()   #  print the current directory (where the R interpreter thinks it is running)
##setwd(...)   # change the current directory to whichever directory holds the .csv file

#Read in csv Format file. hw1_test_file.csv
Table = data.matrix(read.csv('/Users/nnguyen/Documents/_Dev/CS-249_2/hw1/hw1_test_file.csv', header=TRUE))

head(Table)

Distribution = c( "normal", "t", "chi-squared", "lognormal", "exponential", "gamma", "logistic")
Distribution_can_have_negative_values = c( TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE )

for (j in 1:ncol(Table)){ #loop through each column from 1..6 (max columns)
  Dataset = Table[,j] #create each Dataset object for each column
  Dataset_is_nonnegative = !any( Dataset < 0 ) #check if any item in Dataset is negative.
  cat(sprintf("\ntrying Dataset %d:\n", j))
  
  for(i in 1:length(Distribution)){ #loop through each item in Distirbution from 1..7
    dist_name = Distribution[i] #set distribution name
    if(Distribution_can_have_negative_values[i] || Dataset_is_nonnegative){
      # don't try to fit a nonnegative distribution to data that is negative
      
      if (dist_name == "chi-squared") {
        # fitdistr requires special handling of chi-squared
        fit = suppressWarnings( fitdistr( Dataset, dist_name,
                                          list(df=round(mean(Dataset))), method="BFGS" ) )
      } else {
        fit = suppressWarnings( fitdistr( Dataset, dist_name ) )
      }
      
      #print(fit)
      
      fitted_parameters = fit$estimate
      log_likelihood = fit$loglik
      
      # we round the parameter values so that they are integers.
      parameter_value_string = paste(round(fitted_parameters), collapse=" ")
      
      # print integer parameters
      cat(sprintf("%s %s\n", dist_name, parameter_value_string))
      
      # To show how good the fit is, we also print the log-likelihood here
      cat(sprintf("               log-likelihood = %f\n", log_likelihood))
      
      # The optimal distribution is the one with maximum-likelihood
      #  (and:  maximum-likelihood == maximum-log-likelihood).
      # The optimal distribution needs to be tracked here .............
      
    }
    
  }
  
}

