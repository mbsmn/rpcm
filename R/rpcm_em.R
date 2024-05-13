## noch benoetigt (Code aus verhelst_kamphuis_2009_EM.R Funktion fit_gamma_counts fuer pars)
## fit Gamma distribution to count data as in V&K (2009)
## fit Gamma distribution to count data as in V&K (2009)
# Funktion wird in rpcm_em verwendet
fit_gamma_counts <- function(x, freq = NULL, start = "moment",
                             id_constant = 1,
                             terminate = 1e-05){
  n <- length(x)
  if(is.null(freq)){
    freq <- rep(1, length(x))
  }
  if(start[1] == "moment"){
    my_mean <- weighted.mean(x, freq)
    my_var <- sum(freq*((x-my_mean)^2))/(sum(freq)-1)
    rate <- my_mean/(my_var*id_constant)
    shape <- my_mean*rate
  }else{
    shape <- start[1]
    rate <- start[2]
  }
  hess_rr <- hess_ss <- hess_rs <- score_rate <- score_shape <- numeric(n)
  oldpars <- pars <- c(shape, rate)
  crit <- Inf
  while(crit > terminate){
    for(i in 1:n){
      score_shape[i] <- log(rate/(id_constant + rate))
      hess_ss[i] <- 0
      if(x[i]>0){
        score_shape[i] <- score_shape[i] +
          sum(1/(shape + 0:(x[i]-1)))
        hess_ss[i] <- - sum(1/(shape + 0:(x[i]-1))^2)
      }
    }
    score_rate <- shape/rate - (x + shape)/(id_constant + rate)
    hess_rr <- - shape/rate^2 + (x + shape)/(id_constant + rate)^2
    hess_rs <- rep(1/rate - 1/(id_constant + rate), n)
    scores <- c(sum(score_shape * freq),
                sum(score_rate * freq))
    hess <- matrix(c(sum(hess_ss * freq),
                     sum(hess_rs * freq),
                     sum(hess_rs * freq),
                     sum(hess_rr * freq)),
                   ncol = 2)
    pars <- pars - as.numeric(solve(hess) %*% scores)
    shape <- pars[1]
    rate <- pars[2]
    crit <- max(abs(pars - oldpars))
    oldpars <- pars
  }
  pars
}

rpcm_em <- function(X, tau, max.iter = 1000, terminate = 1e-05, id_constant = 1,
                    verbose = TRUE){
  fit_rpcm <- rpcm_cml(X, tau, id_constant = id_constant)
  n <- dim(X)[1]
  delta <- id_constant ## call that delta as in V&K (2009)

  ## define log-lik function
  loglik <- function(){
    #    prob1 <- delta/(delta + rate1)
    #    prob2 <- delta/(delta + rate2)
    #    sum(log(p*dnbinom(rs, shape1, prob1) + (1-p)*dnbinom(rs, shape2, prob2)))
    sum(log(p*dgamma(rs, shape1, rate1) + (1-p)*dgamma(rs, shape2, rate2)))
  }

  rs <- rowSums(X)
  trs <- table(rs)
  obs_s <- as.numeric(names(trs))
  r12s <- numeric(length(obs_s))

  gamma_pars <- fit_gamma_counts(rs, id_constant = id_constant,
                                 terminate = terminate) ## to get starting values
  ## initial parameters
  rate1 <- rate2  <- gamma_pars[2]
  shape1 <- 0.8 * gamma_pars[1]
  shape2 <- 1.2 * gamma_pars[1]
  p <- .5
  #  p <- runif(1)
  #  print(p)
  pars <- oldpars <- c(p, shape1, rate1, shape2, rate2)

  niter <- 0
  crit <- Inf
  while(crit > terminate & !niter >= max.iter){
    niter <- niter + 1
    i <- 1
    for(s in obs_s){
      r12s[i] <- exp(sum(log((shape1 + 0:(s-1))/(shape2 + 0:(s-1))))) *
        (rate1^shape1*(delta + rate2)^(s+shape2)) /
        (rate2^shape2*(delta + rate1)^(s+shape1))
      i <- i + 1
    }
    r12s <- r12s * p/(1-p)

    P_C_equals_1_cond_s <- r12s/(1+r12s)
    P_C_equals_2_cond_s <- 1 - P_C_equals_1_cond_s

    exp_freq_1 <- as.numeric(trs) * P_C_equals_1_cond_s
    exp_freq_2 <- as.numeric(trs) * P_C_equals_2_cond_s

    ## Update p
    p <- sum(exp_freq_1)/n

    ## update Gamma parameters
    gp1 <- fit_gamma_counts(obs_s, freq = exp_freq_1,
                            id_constant = id_constant, terminate = terminate)
    gp2 <- fit_gamma_counts(obs_s, freq = exp_freq_2,
                            id_constant = id_constant, terminate = terminate)
    shape1 <- gp1[1]
    rate1 <- gp1[2]
    shape2 <- gp2[1]
    rate2 <- gp2[2]

    pars <- c(p, shape1, rate1, shape2, rate2)

    ## update log-lik
    ll <- loglik()
    ## print log-lik
    if(verbose){print(paste("Iteration:", niter,
                            "log-likelihood:", ll))}
    #    crit <- max(abs(pars - oldpars))
    crit <- abs(pars[1] - oldpars[1])
    oldpars <- pars
  }

  if(crit > terminate){warning("Reached maximum number of iterations. Consider increasing max.iter.")}

  if(niter == max.iter){warning("Reached maximum number of iterations. Consider increasing max.iter.")}

  return(list(fit_rpcm = fit_rpcm,
              pars = pars, niter = niter,
              terminat = terminate, max.iter = max.iter, crit = crit, ll = ll))
}
