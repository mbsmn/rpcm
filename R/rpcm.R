# load packages:
library(tidyr)
library(lme4)


# main model fitting function
rpcm <- function(data, engine, time_limit = NULL,
                 control = list(cml_id_constant = 1,
                                glmer_control = glmerControl(),
                                em_max_iter = 1000,
                                em_terminate = 1e-05,
                                em_verbose = FALSE)) {
  # time_limit should be a vector as long as the number of items
  if(!is.null(time_limit) & (length(time_limit) != ncol(data))){
    stop("time_limit should be a vector as long as the number of items")
  }

  ## input checks

  # data
  if(is.null(data)){
    warning("data must be specified")
  }

  # count data
  # from R Documentation of integer
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){abs(x - round(x)) < tol}
  for(i in 1:ncol(data)){
    if(sum(!is.wholenumber(data[,i])) > 0){
      stop("item responses should be count data")
    }
  }

  # data should be not negative
  for(i in 1:ncol(data)){
    if(any(data[,i]) < 0){
      stop("item responses should not be negative")
    }
  }

  # remove rows with missing data
  for(i in 1:ncol(data)){
    if(any(is.na(df[,i])) == TRUE){
      data <- data[, -i]
      warning("rows with missing values were removed")
    }
  }


  ## data preparation

  # number of items
  M <- ncol(data)
  #number of persons
  N <- nrow(data)

  if (is.null(time_limit)) {
    time_limit <- rep(1,M)
  }

  # fuer glmer
  # convert data from wide format to long format

  if (engine == "glmer") {
    data_long <- pivot_longer(data, everything(), cols_vary = "slowest",
                              values_to = "score")
    #add id
    id <- rep(1:N, M)
    #add log_time_limit
    log_time_limit <- rep(log(time_limit), each = N)
    data_long <-  cbind(data_long, id, log_time_limit)
    names(data_long) <- c("item", "score", "id","log_time_limit")
    data_long$item <- as.factor(data_long$item)
  }

  # fuer cml und em
  # X und tau benoetigt
  # X mit data ueberprueft, tau wird auf 1-Vektor gesetzt, wenn
  # nicht vorhanden @Marie: soll hier für cml und em eine
  # Warnung eingefuegt werden, dass der 1-Vektor verwendet wird?

  # fuer glmer

  # benoetigte Variablen (im Datensatz)
  # @Marie: Ich glaube glmer benoetigt nur data_long

  # teilweise aus reshape der Datenvorverarbeitung
  # score
  # person = person ID
  # timelimit = Vektor fuer timelimit pro Item (fit2)
  # subset2

  # model fit
  if (engine == "cml") {
    fit <- rpcm_cml(
      X = data,
      tau = time_limit,
      id_constant = control$cml_id_constant)
  } else if (engine == "glmer") {
    fit <- rpcm_glmer(data_long = data_long)
  } else if (engine == "em") {
    fit <- rpcm_em(
      X = as.matrix(data),
      tau = time_limit,
      max.iter = control$em_max_iter,
      terminate = control$em_terminate,
      id_constant = control$cml_id_constant,
      verbose = control$em_verbose)
  }

  # TODO prep for output: ich denke am besten eine liste mit den elementen wie unten
  # angeregt, die muessten jeweils aus dem jeweiligen fit objekt noch herausgezogen werden
  # bitte - das ist schoener, wenn wir das hier so clean herausgeben und einheitlich,
  # egal welchen engine wir verwenden

  if (engine == "cml") {
    item_params <- fit$sigma
    inference <- NA
    estimation <- list(AIC = fit$AIC,
                       BIC = fit$BIC,
                       se.theta = fit$se.theta,
                       se.sigma = fit$se.sigma,
                       chisq = fit$chisq,
                       terminate = NA,
                       niter = NA,
                       max.iter = NA,
                       crit = NA,
                       ll = NA)
  } else if (engine == "glmer") {
    item_params <- fixef(fit)
    inference <- summary(fit)$coefficients
    estimation <- summary(fit)$AICtab      #@Marie: ist das richtig?
  } else if (engine == "em") {
    item_params <- fit$fit_rpcm$sigma
    inference <- NA
    estimation <- list(AIC = fit$fit_rpcm$AIC,
                       BIC = fit$fit_rpcm$BIC,
                       se.theta = fit$fit_rpcm$se.theta,
                       se.sigma = fit$fit_rpcm$se.sigma,
                       chisq = fit$fit_rpcm$chisq ,
                       terminate = fit$terminat,
                       niter = fit$niter,
                       max.iter = fit$max.iter,
                       crit = fit$crit,
                       ll = fit$ll)
  }



  out <- list(
    engine = engine,
    time_limit = time_limit,
    item_params = item_params,
    inference = inference,
    estimation = estimation
    # TODO weiter hinzufuegen, wir brauchten:
    # item_params: das sind die fixed effects aus glmer und die sigmas aus
    # inference: hier bitte eine Tabelle aus Standardfehlern, Teststatistiken und p-Werten
    # das haben wir autoamtisch fuer glmer, bei den anderen beiden koennen wir hier erstmal NA
    # outoutten und muessen dann noch mal igrendwann ueberlegen, wie wir das da am besten bekommen
    # estimation: hier ruhig als liste einfach als output was die jeweiligen engines an output
    # ueber den estimation prozess geben, vielelicht auch als vereinheitlichte subliste - so wie du
    # meinst, dass es besser ist. so was wie zb konvergenz, die anzahl der em iterationen bei em, etc.
  )
  class(out) <- "rpcmfit"
  return(out)
}
