# main model fitting function
rpcm <- function(data, engine, time_limit = NULL,
                 control = list(cml_id_constant = 1,
                                glmer_control = glmerControl(),
                                em_max_iter = 1000,
                                em_terminate = 1e-05,
                                em_verbose = FALSE)) {
  # time_limit should be a vector as long as the number of items

  # TODO input checks

  # count data



  # data preparation

  # number of items
  M <- ncol(data)

  if (is.null(time_limit)) {
    time_limit <- rep(1,M)
  }

  # fuer glmer
  # convert data from wide format to long format
  # TODO Variablennamen anpassen
  # TODO hier bitte einmal gucken wie die sachen benannt sein muessen
  if (engine == "glmer") {
    data$id <- rownames(data)
    data_long <- reshape(data, varying = names(data),
                         v.names = "score",
                          timevar = "subtest2", times = names(ct),
                          idvar = "id", direction = "long")
    # FIXME: wir muessten hier dafuer sorgen dass die spalte mit den item-namen
    # dann bitte "item" heisst - ich weiss bei reshape nur leider nie, welches
    # der argumente das ist @Loreen: Hast du da ne idee?
    # TODO time_limit Werte einsetzen so dass fuer jeden item-eintrag in
    # data_long das korresponiderende time limit des Items dran ist.
    # am besten direkt das logarithmierte time limit einsetzen, bitte
    # log_time_limit nennen
  }



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

  # model fit
  if (engine == "cml") {
    fit <- rpcm_cml(
      X = data,
      tau = time_limit,
      id_constant = control$cml_id_constant)
  } else if (engine == "glmer") {
    fit <- rpcm_glmer() # TODO argumente einfuegen
  } else if (engine == "em") {
    fit <- rpcm_em(
      X = data,
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
  out <- list(
    engine = engine,
    time_limit = time_limit
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
