log_prob <- function(X, beta){
  pk <- exp(X %*% beta) / rowSums(exp(X %*% beta))
  return(pk)
}

beta_update <- function(X, Pmatrix, eta, beta, lambda, n){
  new_beta <- beta - eta * solve(t(X) %*% diag(Pmatrix*(1 - Pmatrix), nrow = n, ncol = n) %*% X + 
                       lambda*diag(1, nrow = n, ncol + n)) %*% (t(X) %*% (Pmatrix - 1) + lambda * beta)
  return(new_beta)
}



# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(sum(X[, 1]) != nrow(X)){
    stop("First column of X is not 1's")
  } else if(sum(Xt[, 1]) != nrow(Xt)){
    stop("First column of Xt is not 1's")
  } 
  
  # Check for compatibility of dimensions between X and Y
  if(nrow(X) != length(Y)){
    stop("Number of rows X does not match length of Y")
  }
  
  # Check for compatibility of dimensions between Xt and Yt
  if(nrow(Xt) != length(Yt)){
    stop("Number of rows of Xt does not match length of Yt")
  }
  
  # Check for compatibility of dimensions between X and Xt
  if(ncol(X) != ncol(Xt)){
    stop("Number of columns of X and Xt do not match")
  }
  
  # Check eta is positive
  if(eta <= 0){
    stop("eta needs to be positive")
  }
  
  # Check lambda is non-negative
  if(lambda < 0){
    stop("lambda needs to be non-negative")
  }
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if(is.null(beta_init)){
    beta_init <- matrix(0, nrow = ncol(X), ncol = sort(unique(Y))[length(unique(Y))])
    return(beta_init)
  } else if(array(dim(beta_init)) != array(c(nrow(X, length(unique(Y)))))){
    stop("Dimension of beta_init needs to be n \u00D7 p")
  }
  
  ## Calculate corresponding pk, objective value f(beta_init), training error and 
  ## testing error given the starting point beta_init
  ##########################################################################
  n <- nrow(X)
  p <- ncol(X)
  K <- length(unique(Y))
  
  beta <- matrix(nrow = p, ncol = K)
  beta <- beta_init
  
  Pmatrix <- c()
  
  fbeta <- ((lambda / 2) * sum(beta^2))
  for(i in 1:n){
    for(j in 1:K-1){
      if(Y[i] == j){
        Pmatrix[i] <- log_prob(X[i], beta)
        fbeta <- fbeta - log_prob(X[i], beta)
      }
    }
  }
  
  
  
  
  
  pk <- exp(X %*% beta_init) / rowSums(exp(X %*% beta_init))
  fbeta <- -sum(log(pk)) + ((lambda / 2) * sum(beta^2))
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  for(i in 1:numIter){
    beta_new <- beta - eta * (t(X) %*% X + lambda %*% diag(1, nrow = n, ncol = n))^-1 %*% (t(X) + lambda * beta)
  }
  
  # Within one iteration: perform the update, calculate updated objective function 
  ## and training/testing errors in %
  
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}



