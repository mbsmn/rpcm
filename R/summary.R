#' Summary for cirtfit objects.
#'
#' @param x A cirtfit object.
#' @export
summary.rpcmfit <- function(x) {
  # engine
  cat(paste0("Engine: ", x$engine, "\n", "\n"))

  # time limit
  cat(paste0("Time Limit: ", "\n"))
  print(x$time_limit)

  #item estimation
  cat(paste0("\n", "Parameter Estimation:", "\n"))
  print(x$item_params)

  cat(paste0("\n", "Inference", "\n"))
  print(x$inference)

  # noch hinzufuegen: logLik, deviance, df.resid fuer glmer
  # noch hinzufuegen se. theta, se.sigma, chisq,
  # terminate, niter, maxiter, crit, ll
  #estimation.output <- list(AIC = x$estimation$AIC,
  #                          BIC = x$estimation$AIC,
  #                          )

  cat(paste0("\n", "Estimation", "\n"))
  print(x$estimation)
}
