# TODO hier den code einfuegen aus verhelst_kamphuis_2009_EM.R

## noch benoetigt (Code aus verhelst_kamphuis_2009_EM.R Funktion fit_gamma_counts fuer pars)
## fit Gamma distribution to count data as in V&K (2009)


rpcm_em <- function(X,tau, max.iter = 1000, terminate = 1e-05, id_constant = 1,
                    verbose = TRUE){
  fit_rpcm <- rpcm(X,tau, id_constant = id_constant)
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
  print(p)
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

  list(fit_rpcm = fit_rpcm,
       pars = pars, terminat = terminate, max.iter = max.iter, crit = crit, ll = ll)
}
