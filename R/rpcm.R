# main model fitting function
rpcm <- function(data, engine) {

  # TODO input checks

  # count data




  # TODO data preparation

  # fuer em
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
    hess_rr <- hess_ss <- hess_rs <-
      score_rate <- score_shape <- numeric(n)
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

  # fuer glmer
  # convert data from wide format to long format
  # TO DO Variablennamen anpassen
  ctl <- reshape(ct, varying = names(ct), v.names = "score",
                 timevar = "subtest2", times = names(ct),
                 idvar = "person", direction = "long")
  ctl$unlimit <- ifelse(grepl("unlimit", ctl$subtest), "yes", "no") # make a neat variable to
  # know which part has no time limit (yes = no time limit)
  ctl$subtest <- gsub("ctestlimit", "", gsub("un", "", ctl$subtest2)) # make numerical variable
  # for subtest
  head(ctl) # first few lines: data merely reorganized
  tail(ctl) # last few lines


  # TODO data checks

  # fuer cml und em
  # X und tau benoetigt

  # fuer glmer

  # benoetigte Variablen (im Datensatz)
  # teilweise aus reshape der Datenvorverarbeitung
  # score
  # person = person ID
  # timelimit = Vektor fuer timelimit pro Item (fit2)
  # subset2



  # TODO ueber das engine argument geben wir hier dann die verschiedenen
  # fitting optionen hinein
  if (engine == "cml") {
    # TODO hier code einfuegen um das modell mit rpcm_cml zu fitten
    fit <- rpcm_cml()# TODO
  } else if (engine == "glmer") {
    # TODO hier code einfuegen um das modell mit rpcm_glmer zu fitten
    fit <- rpcm_glmer() # TODO
  } else if (engine == "em") {
    # TODO hier code einfuegen um das modell mit rpcm_em zu fitten
    fit <- rpcm_em()
  }

  # TODO prep for output
  return(fit)
}
