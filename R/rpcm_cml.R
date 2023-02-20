# TODO hierein kommt der code aus rpcm.R
rpcm_cml <- function(X, tau, id_constant = 1){
  X <- as.matrix(X)
  I <- ncol(X)
  J <- nrow(X)
  theta <- rowSums(X)/id_constant
  delta <- colSums(X)/sum(X)
  sigma <- id_constant*delta/tau

  se.sigma <- sigma/(sqrt(colSums(X))*id_constant)
  se.theta <- id_constant*sqrt(theta) ## short for hat theta / sqrt(rowSums(theta))

  Theta <- matrix(theta, ncol = ncol(X), nrow = nrow(X))
  Delta <- matrix(sigma*tau, ncol = ncol(X), nrow = nrow(X), byrow = TRUE)
  lambda <- Theta*Delta
  chisq <- ((X - lambda)^2)/lambda
  colnames(chisq) <- colnames(X)

  ## calculate AIC
  npars <- ncol(X) + nrow(X) - 1 # -1 for identification constraint
  loglik  <- sum(dpois(as.vector(X), as.vector(lambda), log = TRUE))
  AIC <- -2*loglik + 2*npars
  BIC <- -2*loglik + log(I*J)*npars

  return(list(X=X, tau = tau, theta = theta, sigma = sigma,
              se.theta = se.theta, se.sigma = sigma,
              lambda = lambda, chisq = chisq, AIC = AIC, BIC = BIC,
              id_constant = id_constant))
}
