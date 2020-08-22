# Function to find the significance of the variable
#Function for getting the significance of each variable
# Chi square test -- when both the variables to compare are categorical
# t-test -- when one of the variable is numerical and the other is categorical
# Function modified to find the factors by itself
# function to automatically convert a numerical categorical variable to factors
#Function works only for binary classification problem
#Its an addon to the previous function where the function interprets both factor and numerical variables
# Still the target variable has to be provided 
setwd('C:\\Users\\anand\\Downloads')
fram <- read.csv('framingham.csv')

signif_var_modified <- function(data,target_var,thresh)
{
  num_to_factor <- function(data,thresh) #Function to identify factor type variables
  {
    
    for (i in 1:(ncol(data)-1))
    {
      if(is.integer(data[,i])|is.numeric(data[,i]))
      {
        if(length(unique(data[,i])) < thresh)
        {
          data[,i] <- as.factor(data[,i])
        }
        
      }
    }
    return(data)
  }
  data1 <- num_to_factor(data,thresh) # variables identified as factors
  
  
  # function to identify the factor and numeric variable names
  
  ident_num_var <- function(data)
  {
    k=1
    num_var <- c()
    for (i in 1:(ncol(data)-1))
    {
      if(is.numeric(data[,i]) | is.integer(data[,i]))
      {
        num_var[k] <- c(names(data)[i])
        k=k+1
      }
    }
    return(num_var)
  }
  
  ident_factor_var <- function(data)
  {
    j=1
    factor_var <- c()
    for (i in 1:(ncol(data)-1))
    {if(is.factor(data[,i]))
    {
      factor_var[j] <- c(names(data)[i])
      j=j+1
    }
    }
    return(factor_var)
  }
  
  num_var <- ident_num_var(data1)
  factor_var <- ident_factor_var(data1)
  # function to check significance of the variables
  chi_square_test <- function(data,factor_var,target_var)
  {
    l = 1
    chi_significance = c()
    for (i in factor_var)
    {
      chi_significance[l] <-chisq.test(data[,i],data[,target_var])['p.value']    
      l=l+1
    }
    chi_significance <- as.matrix(chi_significance)
    rownames(chi_significance) <- factor_var
    colnames(chi_significance) = 'p-value'
    return(chi_significance)
    
  }
  
  t_test <- function(data,num_var,target_var)
  {
    k=1
    t_significance <- c()
    for (i in num_var)
    {
      t_significance[k] <- t.test(data[i][data[target_var]== 0],data[i][data[target_var]==1])['p.value']
      k=k+1
    }
    t_significance <- as.matrix(t_significance)
    row.names(t_significance) = num_var
    colnames(t_significance) = 'p-value'
    return(t_significance)
  }
  print('Numerical Variable')
  print(t_test(data1,num_var,target_var))
  print('Categorical Variable')
  print(chi_square_test(data1,factor_var,target_var))
}

signif_var_modified(fram,'TenYearCHD',5)