# This is a script to save your own tests for the function
source("FunctionsLR.R")

temp <- matrix(1, nrow= 4)
temp2 <- matrix(1, nrow = 6)

LRMultiClass(X, Y, Xt,Yt)




n <- nrow(X)
p <- ncol(X)
K <- length(unique(Y))

beta <- matrix(nrow = p, ncol = K)
beta_init <- matrix(0, nrow = ncol(X), ncol = K)
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

dim(matrix(Pmatrix, ncol = 1) - 1)

test <- matrix(rowSums(beta)) - eta * solve(t(X) %*% diag(Pmatrix*(1 - Pmatrix), nrow = n, ncol = n) %*% X + 
                     lambda*diag(1, nrow = p, ncol = p)) %*% (t(X) %*% (matrix(Pmatrix, ncol = 1) - 1) + lambda * matrix(rowSums(beta)))

for (k in 0:(K-1)) {
  Pk <- pk[, k + 1]
  Wk <- diag(Pk * (1 - Pk))  # n x n diagonal matrix
  indicator_k <- as.numeric(y == k)
  
  # Hessian and gradient update
  H_k <- t(X) %*% diag(Pk * (1 - Pk)) %*% X + lambda * diag(p)
  grad_k <- t(X) %*% (Pk - as.numeric(Y == k)) + lambda * beta[, k + 1]
  
  # Damped Newton update
  beta[, k + 1] <- beta[, 2] - eta * solve(t(X) %*% diag(Pk * (1 - Pk)) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% (Pk - as.numeric(Y == k)) + lambda * beta[, 2] 
}

dim(pk)

for(k in 0:(K-1)){
  matrix(rowSums(beta)) - eta * solve(t(X) %*% diag(pk*(1 - pk), nrow = n, ncol = n) %*% X + 
                                        lambda*diag(1, nrow = p, ncol = p)) %*% (t(X) %*% (matrix(pk, ncol = 1) - 1) + lambda * matrix(rowSums(beta)))
  
}







pk <- exp(X %*% beta_init) / rowSums(exp(X %*% beta_init))
fbeta <- -sum(log(pk)) + ((lambda / 2) * sum(beta^2))









