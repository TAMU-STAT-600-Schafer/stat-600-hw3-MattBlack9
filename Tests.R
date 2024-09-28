# This is a script to save your own tests for the function
source("FunctionsLR.R")

d1 <- matrix(1,nrow = 10, ncol = 10)
d2 <- matrix(2,nrow = 10, ncol = 10)
d3 <- matrix(1, nrow = 5, ncol = 5)
a1 <- array(1, dim = legnth(10))
a2 <- array(1, dim = length(15))
b1 <- matrix(1, nrow = 2, ncol = 2)

# This checks to see if an error is thrown if X and Xt first columns are not 1s
LRMultiClass(d1, a1, d2, a1)
LRMultiClass(d2, a1, d1, a1,)

# This makes sure the dimensionality of X and Y match
LRMultiClass(d1, a2, d1, a1)

# This makes sure the dimensionality of Xt and Yt match
LRMultiClass(d1, a1, d1, a2)

# This makes sure the dimensionality of X and Xt match
LRMultiClass(d1, d3, a1, a1)

# This is to make sure an error is thrown if eta is zero or negative
LRMultiClass(d1, a1, d1, a1, eta = 0)
LRMultiClass(d1, a1, d1, a1, eta = -1)

# This is to make sure an error is thrown if lambda is negative
LRMultiClass(d1, a1, d1, a1, lambda = -1)

# This is to make sure beta_init is the right dimensions
LRMultiClass(d1, a1, d1, a1, beta_init = b1)






