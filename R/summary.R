#' Summary for rpcmfit objects.
#'
#' @param x A rpcmfit object.
#' @export
summary.rpcmfit <- function(x) {
  # engine
  cat(paste0("Engine: ", x$engine, "\n", "\n"))

  # time limit
  cat(paste0("Time Limit: ", "\n"))
  print(x$time_limit)

  #item estimation
  cat(paste0("\n", "Item Estimation:", "\n"))
  print(x$item_params)

  # inference
  cat(paste0("\n", "Inference", "\n"))
  print(x$inference)

  if(x$engine == "cml"){
    AIC <- x$estimation$AIC
    BIC <- x$estimation$BIC
    logLik <- NA
    se.theta <- x$estimation$se.theta
    se.sigma <- x$estimation$se.sigma
    chisq <- x$estimation$chisq
    terminate <- NA
    niter <- NA
    max.iter <- NA
    crit <- NA
    deviance <- NA
    df.resid <- NA
  }
  if(x$engine == "em"){
    AIC <- x$estimation$AIC
    BIC <- x$estimation$BIC
    logLik <- x$estimation$ll
    se.theta <- x$estimation$se.theta
    se.sigma <- x$estimation$se.sigma
    chisq <- x$estimation$chisq
    terminate <- x$estimation$terminate
    niter <- x$estimation$niter
    max.iter <- x$estimation$max.iter
    crit <- x$estimation$crit
    deviance <- NA
    df.resid <- NA
  }
  if(x$engine == "glmer"){
    AIC <- x$estimation["AIC"]
    BIC <- x$estimation["BIC"]
    logLik <- x$estimation["logLik"]
    se.theta <- NA
    se.sigma <- NA
    chisq <- NA
    terminate <- NA
    niter <- NA
    max.iter <- NA
    crit <- NA
    deviance <- x$estimation["deviance"]
    df.resid <- x$estimation["df.resid"]
  }

  cat(paste0("\n", "Estimation:", "\n"))
  cat(paste0("  ", "AIC: ", round(AIC, 6), "\n"))
  cat(paste0("  ", "BIC: ", round(BIC, 6), "\n"))
  cat(paste0("  ", "Log Likelihood: ", round(logLik, 6), "\n"))
  cat(paste0("  ", "Deviance: ", round(deviance, 6), "\n"))
  cat(paste0("  ", "Degrees of freedom: ", round(df.resid, 6), "\n"))
  cat(paste0("\n", "  ", "Standard Error theta: ", "\n", "\n"))
  print(se.theta)
  cat(paste0("\n", "  ","Standard Error sigma: ", "\n", "\n"))
  print(se.sigma)
  cat(paste0("\n", "  ", "Chi Square: ", "\n", "\n"))
  print(chisq)
  cat(paste0("\n", "  ", "terminate: ", round(terminate, 6), "\n"))
  cat(paste0("  ", "niter: ", round(niter, 0), "\n"))
  cat(paste0("  ", "max.iter: ", round(max.iter, 0), "\n"))
  cat(paste0("  ", "crit: ", round(crit, 6), "\n"))
}
