# This is a script to save your own tests for the function
source("FunctionsLR.R")

temp <- matrix(1, nrow= 4)
temp2 <- matrix(1, nrow = 6)

LRMultiClass(X, Y, Xt,Yt)




n <- nrow(X)
p <- ncol(X)
K <- length(unique(Y))

beta <- matrix(nrow = p, ncol = K)
beta_init <- matrix(0, nrow = ncol(X), ncol = sort(unique(Y))[length(unique(Y))])
beta <- beta_init

Pmatrix <- c()

fbeta <- ((1 / 2) * sum(beta^2))
for(i in 1:n){
  for(j in 1:K-1){
    if(Y[i] == j){
      Pmatrix[i] <- log_prob(X[i,], beta)
      fbeta <- fbeta - log_prob(X[i,], beta)
    }
  }
}

diag(Pmatrix*(1 - Pmatrix), nrow = n, ncol = n)[3,3]


lambda <- 1
eta <- 0.1


beta - eta * solve(t(X) %*% diag(Pmatrix*(1 - Pmatrix), nrow = n, ncol = n) %*% X + 
                     lambda*diag(1, nrow = p, ncol = p)) %*% (t(X) %*% (Pmatrix - 1) + lambda * beta)





